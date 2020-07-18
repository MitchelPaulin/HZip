import qualified Data.ByteString               as B
import           System.Environment
import           Control.Monad
import           GHC.Word
import           LZ77Common

emptyBit :: GHC.Word.Word8 
emptyBit = 255
bufferSize :: Int 
bufferSize = 254
lookaheadSize :: Int
lookaheadSize = 128

main :: IO ()
main = do
    (op : file : _) <- getArgs

    bytes           <- B.readFile file
    when
        (op == "-e")
        (B.writeFile
            (file ++ LZ77Common.fileExtension)
            (B.concat
                (map packEncodingIntoByteStream
                     (LZ77Common.getLZ77Encoding bytes 0 bufferSize lookaheadSize)
                )
            )
        )
    when
        (op == "-d")
        (B.writeFile
            (take (length file - length LZ77Common.fileExtension) file)
            (B.pack $ decodeLZ77 [] (B.unpack bytes))
        )

-- | Convert from the encoded LZ77 stream back to the original file
decodeLZ77 :: [GHC.Word.Word8] -> [GHC.Word.Word8] -> [GHC.Word.Word8]
decodeLZ77 decoded (seekBack : wordSize : xs) = if seekBack == emptyBit
    then decodeLZ77 (decoded ++ [wordSize]) xs
    else decodeLZ77 (decoded ++ slice startSlice endSlice decoded) xs
    where
    startSlice = length decoded - fromIntegral seekBack
    endSlice   = startSlice + fromIntegral wordSize
decodeLZ77 decoded _                         = decoded


-- | Takes an LZ77 encoding pair and produces the corresponding byte string object
-- | emptyBit means the next Word8 will be a literal 
packEncodingIntoByteStream :: LZ77EncodingPair -> B.ByteString
packEncodingIntoByteStream (Nothing, character) =
    B.pack [fromIntegral emptyBit, B.head character]
packEncodingIntoByteStream (Just distance, string) =
    B.pack $ map fromIntegral [distance, B.length string]
