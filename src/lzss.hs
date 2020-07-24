import qualified Data.ByteString               as B
import           Data.List.Split
import           System.Environment
import           Control.Monad
import           LZ77Common
import           BitsCommon
import           GHC.Word

bufferPower :: Int
bufferPower = 15
lookaheadPower :: Int
lookaheadPower = 8
bufferSize :: Int
bufferSize = 32767 -- 2^15 - 1
lookaheadSize :: Int
lookaheadSize = 255 -- 2^8 - 1

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
            (B.pack
                (decodeLZSS (concatMap wordToBits (B.unpack bytes)) [])
            )
        )

-- | The 'decodeLZSS' function takes an array of bits representing a file encoded in lzss and expands it to its original representation
decodeLZSS :: Bits -> [GHC.Word.Word8] -> [GHC.Word.Word8]
decodeLZSS (x : xs) decoded = if length xs >= 8
    then if x
        then decodeLZSS (drop byteSize xs) (decoded ++ [bitsToWord $ take byteSize xs])
        else decodeLZSS (drop (lookaheadPower + bufferPower) xs) 
                        (decoded ++ LZ77Common.slice startSlice endSlice decoded)
    else decoded
  where
    startSlice =
        length decoded - bitsToInt (LZ77Common.slice lookaheadPower (lookaheadPower + bufferPower) xs)
    endSlice = startSlice + bitsToInt (take lookaheadPower xs)
decodeLZSS [] decoded = decoded

-- | The 'packEncodingIntoLZSSByteStream' takes the encoding pairs and packs it into a bit stream
packEncodingIntoLZSSByteStream :: LZ77Common.LZ77EncodingPair -> Bits
packEncodingIntoLZSSByteStream (Nothing, character) =
    True : wordToBits (head $ B.unpack character)
packEncodingIntoLZSSByteStream (Just distance, string) =
    False : padWithZeros byteSize (intToBits $ B.length string) ++ padWithZeros
        bufferPower
        (intToBits distance)
