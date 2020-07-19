import qualified Data.ByteString               as B
import qualified Data.Bits                     as Bits
import           Data.List.Split
import           System.Environment
import           GHC.Word
import           Control.Monad
import           LZ77Common

type Bit = Bool
type Bits = [Bit]

byteSize :: Int
byteSize = 8
bufferSize :: Int
bufferSize = 2 ^ 15 - 1
lookaheadSize :: Int
lookaheadSize = 2 ^ 8 - 1

main :: IO ()
main = do
    (op : file : _) <- getArgs

    bytes           <- B.readFile file

    when
        (op == "-e")
        (B.writeFile
            (file ++ LZ77Common.fileExtension)
            (B.pack
                (map
                    bitsToWord
                    (padListTo8
                        (chunksOf
                            byteSize
                            (concatMap
                                packEncodingIntoLZSSByteStream
                                (LZ77Common.getLZ77Encoding bytes
                                                            0
                                                            bufferSize
                                                            lookaheadSize
                                )
                            )
                        )
                    )
                )
            )
        )
    when
        (op == "-d")
        (B.writeFile
            (take (length file - length LZ77Common.fileExtension) file)
            (B.pack $ map
                bitsToWord
                (chunksOf
                    8
                    (decodeLZSS [] (concatMap wordToBits (B.unpack bytes)))
                )
            )
        )
    when (op == "-t") (writeFile "out.txt" (show $ chunksOf 8 (decodeLZSS [] (concatMap wordToBits $ B.unpack bytes))))

decodeLZSS :: Bits -> Bits -> Bits
decodeLZSS decoded (x : xs) = if length xs >= 8 then
                                if x
                                then decodeLZSS (decoded ++ take 8 xs) (drop 8 xs)
                                else decodeLZSS
                                (decoded ++ LZ77Common.slice startSlice endSlice decoded)
                                (drop 23 xs)
                            else decoded
  where
    startSlice = length decoded - bitsToInt (LZ77Common.slice 8 24 xs) * byteSize
    endSlice   = startSlice + bitsToInt (take 8 xs) * byteSize
decodeLZSS decoded [] = decoded

packEncodingIntoLZSSByteStream :: LZ77Common.LZ77EncodingPair -> Bits
packEncodingIntoLZSSByteStream (Nothing, character) =
    True : wordToBits (head $ B.unpack character)
packEncodingIntoLZSSByteStream (Just distance, string) =
    False
        :  padWithZeros 8 (intToBits $ B.length string)
        ++ padWithZeros 15 (intToBits distance)

padListTo8 :: [Bits] -> [Bits]
padListTo8 x = init x ++ [reverse $ padWithZeros 8 (reverse $ last x)]

-- | The 'wordToBits' function converts a word8 into a sequence of Trues and Falses representing high and low bits
wordToBits :: GHC.Word.Word8 -> Bits
wordToBits word = padWithZeros 8 res
                  where res = intToBits (fromIntegral word)

-- | The 'bitsToWord' function takes a list of bits and packs them into an 8 bits word
bitsToWord :: Bits -> GHC.Word.Word8
bitsToWord bits = fromIntegral $ bitsToInt bits

-- | The 'boolToBit' function converts a single bit to a boolean
bitsToInt :: Bits -> Int
bitsToInt b = foldr (\x y -> fromEnum x + 2*y) 0 (reverse b)

intToBits :: Int -> Bits
intToBits 0 = [False]
intToBits n = map (> 0) (go n [])
    where go 0 r = r
          go k rs = go (div k 2) (mod k 2:rs)

padWithZeros :: Int -> Bits -> Bits 
padWithZeros n bits = if length bits < n then replicate (n - length bits) False ++ bits
                        else bits
