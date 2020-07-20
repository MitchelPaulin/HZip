import qualified Data.ByteString               as B
import           Data.List.Split
import           System.Environment
import           Control.Monad
import           LZ77Common
import           BitsCommon

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
            (B.pack $ map
                bitsToWord
                (chunksOf
                    byteSize
                    (decodeLZSS [] (concatMap wordToBits (B.unpack bytes)))
                )
            )
        )
    when
        (op == "-t")
        (writeFile
            "out.txt"
            (show $ chunksOf
                8
                (decodeLZSS [] (concatMap wordToBits $ B.unpack bytes))
            )
        )

-- | The 'decodeLZSS' function takes an array of bits representing a file encoded in lzss and expands it to its original representation
decodeLZSS :: Bits -> Bits -> Bits
decodeLZSS decoded (x : xs) = if length xs >= 8
    then if x
        then decodeLZSS (decoded ++ take 8 xs) (drop 8 xs)
        else decodeLZSS
            (decoded ++ LZ77Common.slice startSlice endSlice decoded)
            (drop 23 xs)
    else decoded
  where
    startSlice =
        length decoded - bitsToInt (LZ77Common.slice 8 24 xs) * byteSize
    endSlice = startSlice + bitsToInt (take 8 xs) * byteSize
decodeLZSS decoded [] = decoded

-- | The 'packEncodingIntoLZSSByteStream' takes the encoding pairs and packs it into a bit stream
packEncodingIntoLZSSByteStream :: LZ77Common.LZ77EncodingPair -> Bits
packEncodingIntoLZSSByteStream (Nothing, character) =
    True : wordToBits (head $ B.unpack character)
packEncodingIntoLZSSByteStream (Just distance, string) =
    False : padWithZeros 8 (intToBits $ B.length string) ++ padWithZeros
        15
        (intToBits distance)
