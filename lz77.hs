module LZ77 where
import           Data.List
import           Data.Maybe
import           System.IO
import           System.Environment
import           Data.ByteString.Search
import qualified Data.ByteString               as B
import qualified Data.Bits                     as Bits
import           GHC.Word
import           Control.Monad

-- | If the character is a literal (Nothing, Character)
-- | If a prefix was found (Distance Back, Match)
type LZ77EncodingPair = (Maybe Int, B.ByteString)

bufferSize = 2 ^ 8 - 1
lookaheadSize = 2 ^ 6
emptyBit = 2 ^ 8

fileExtension = ".hzip"

-- main :: IO ()
-- main = do
--     (op : file : _) <- getArgs

--     bytes           <- B.readFile file
--     when
--         (op == "-e")
--         (B.writeFile
--             (file ++ fileExtension)
--             (B.concat (map packEncodingIntoByteStream (getLZ77Encoding bytes 0))
--             )
--         )
--     when
--         (op == "-d")
--         (B.writeFile (take (length file - length fileExtension) file)
--                      (B.pack $ decodeLZ77 [] (B.unpack bytes))
--         )

-- | Convert from the encoded LZ77 stream back to the original file
decodeLZ77 :: [GHC.Word.Word8] -> [GHC.Word.Word8] -> [GHC.Word.Word8]
decodeLZ77 decoded []                         = decoded
decodeLZ77 decoded (seekBack : wordSize : xs) = if seekBack == emptyBit
    then decodeLZ77 (decoded ++ [wordSize]) xs
    else decodeLZ77 newDecoded xs
  where
    startSlice = length decoded - fromIntegral seekBack
    endSlice   = startSlice + fromIntegral wordSize
    newDecoded = decoded ++ slice startSlice endSlice decoded


-- | Takes an LZ77 encoding pair and produces the corresponding byte string object
-- | emptyBit means the next Word8 will be a literal 
packEncodingIntoByteStream :: LZ77EncodingPair -> B.ByteString
packEncodingIntoByteStream (Nothing, character) =
    B.pack [fromIntegral emptyBit, B.head character]
packEncodingIntoByteStream (Just distance, string) =
    B.pack $ map fromIntegral [distance, B.length string]

-- | Takes a ByteString and returns the sequence encoded in LZ77
getLZ77Encoding :: B.ByteString -> Int -> [LZ77EncodingPair]
getLZ77Encoding byteString index = if index < B.length byteString
    then (start, match) : getLZ77Encoding byteString (index + B.length match)
    else []
    where (start, match) = getLongestPrefixInLookahead byteString index

-- | Helper method to get the lz77 encoding at a specific prefix and buffer
getLongestPrefixInLookahead :: B.ByteString -> Int -> LZ77EncodingPair
getLongestPrefixInLookahead byteString prefixIndex = longestPrefix
    buffer
    lookahead
  where
    buffer = sliceByteString (max (prefixIndex - bufferSize) 0)
                             prefixIndex
                             byteString
    lookahead =
        sliceByteString prefixIndex (prefixIndex + lookaheadSize) byteString


-- | The 'longestPrefix' function finds the longest prefix of 'prefix' that exists in the buffer, return Nothing if no such prefix exists
longestPrefix :: B.ByteString -> B.ByteString -> LZ77EncodingPair
longestPrefix buffer prefix
    | B.length prefix <= 1 = (Nothing, prefix)
    | otherwise = if isJust subStrIndex
        then (Just $ B.length buffer - fromJust subStrIndex, prefix)
        else longestPrefix buffer (B.init prefix)
    where subStrIndex = findSubstring prefix buffer


-- | The 'findSubstring' function finds the index of the start of pat in str
findSubstring :: B.ByteString -> B.ByteString -> Maybe Int
findSubstring pat str = if null result
    then Nothing
    else Just $ head result
    where result = Data.ByteString.Search.indices pat str


-- | The 'sliceByteString' function returns a range in a ByteString over (from:to)
sliceByteString :: Int -> Int -> B.ByteString -> B.ByteString
sliceByteString from to xs = B.take (to - from) (B.drop from xs)

-- | The 'slice' function returns a range in a list over (from:to)
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from) (drop from xs)
