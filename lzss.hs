import qualified Data.ByteString               as B
import qualified Data.Bits                     as Bits
import           Data.ByteString.Search
import           Data.List
import           Data.Maybe
import           Data.List.Split
import           System.IO
import           System.Environment
import           GHC.Word
import           Control.Monad
import           LZ77Common

type Bit = Bool
type Bits = [Bit]
byteSize = 8

{-# LANGUAGE BinaryLiterals #-}

masks = [
    0b10000000,
    0b01000000,
    0b00100000,
    0b00010000,
    0b00001000,
    0b00000100,
    0b00000010,
    0b00000001]

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
                    (chunksOf
                        byteSize
                        (concat
                            (map packEncodingIntoLZSSByteStream
                                 (LZ77Common.getLZ77Encoding bytes 0)
                            )
                        )
                    )
                )
            )
        )
    when
        (op == "-t")
        (writeFile "test.txt"
                   (show $ map (bitsToWord . wordToBits) (B.unpack bytes))
        )

packEncodingIntoLZSSByteStream :: LZ77Common.LZ77EncodingPair -> Bits
packEncodingIntoLZSSByteStream (Nothing, character) =
    True : wordToBits (head $ B.unpack character)
packEncodingIntoByteStream (Just distance, string) =
    False : wordToBits distance ++ (wordToBits $ fromIntegral $ B.length string)

-- | The 'wordToBits' function converts a word8 into a sequence of Trues and Falses representing high and low bits
wordToBits :: GHC.Word.Word8 -> Bits
wordToBits word = map (\x -> x Bits..&. word >= 1) masks

-- | The 'bitsToWord' function takes a list of bits and packs them into an 8 bits word
bitsToWord :: Bits -> GHC.Word.Word8
bitsToWord bits = foldr
    ((Bits..|.) . fst)
    0
    (filter snd (zip (map Bits.bit (reverse [0 .. 7])) bits))

-- | The 'boolToBit' function converts a single bit to a boolean
boolToBit :: Bit -> Int
boolToBit b = if b then 1 else 0
