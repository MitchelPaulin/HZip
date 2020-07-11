import Data.List
import Data.Maybe
import System.IO
import System.Environment
import qualified Data.ByteString as B
import qualified Data.Bits as Bits
import GHC.Word
import Control.Monad

bufferSize = 2^7
lookaheadSize = 2^7
emptyBit = 2^7 + 1

main :: IO ()
main = do
    (op:file:_) <- getArgs
    
    -- plain text encoding
    -- bytes <- readFile file
    -- print (getLZ77Encoding bytes 0)
    
    -- byte based encoding
    bytes <- B.readFile file
    when (op == "-e") (B.writeFile (file ++ ".hzip") (B.concat (map packEncodingIntoByteStream (getLZ77Encoding (B.unpack bytes) 0))))
    when (op == "-d") (B.writeFile (drop 4 file) (B.pack $ decodeLZ77 [] (B.unpack bytes)))

decodeLZ77 :: [GHC.Word.Word8] -> [GHC.Word.Word8] -> [GHC.Word.Word8]
decodeLZ77 decoded [] = decoded
decodeLZ77 decoded (index:wordSize:xs) = if index == emptyBit then decodeLZ77 (decoded ++ [wordSize]) xs
                                else decodeLZ77 newDecoded xs 
                                where startSlice = fromIntegral index
                                      endSlice = startSlice + fromIntegral wordSize
                                      newDecoded = decoded ++ slice startSlice endSlice decoded

    
-- | Takes an LZ77 encoding pair and produces the corresponding byte string object
-- | emptyBit means the next Word8 will be a literal 
packEncodingIntoByteStream :: (Maybe Int, [GHC.Word.Word8]) -> B.ByteString
packEncodingIntoByteStream (Nothing, character) = B.pack [fromIntegral emptyBit, head character]
packEncodingIntoByteStream (Just distance, string) = B.pack $ map fromIntegral [distance, length string]

-- | Takes a string of text and returns the sequence encoded in LZ77
getLZ77Encoding :: Eq a => [a] -> Int -> [(Maybe Int, [a])]
getLZ77Encoding byteString index = if index < length byteString 
                                    then (start, match) : getLZ77Encoding byteString (index + length match)
                                    else []
                                    where (start, match) = getLongestPrefixInLookahead byteString index

-- | Helper method to get the lz77 encoding at a specific prefix
getLongestPrefixInLookahead :: Eq a => [a] -> Int -> (Maybe Int, [a])
getLongestPrefixInLookahead byteString prefixIndex = longestPrefix buffer lookahead
                                                where buffer = slice (max (prefixIndex - bufferSize) 0) prefixIndex byteString
                                                      lookahead = slice prefixIndex (prefixIndex + lookaheadSize) byteString


-- | The 'longestPrefix' function finds the longest prefix of 'prefix' that exists in the buffer, return Nothing if no such prefix exists
longestPrefix :: Eq a => [a] -> [a] -> (Maybe Int, [a])
longestPrefix _ [] = (Nothing, [])
longestPrefix _ [x] = (Nothing, [x])
longestPrefix buffer prefix = if isJust subStr then (subStr, prefix)
                                else longestPrefix buffer (init prefix)
                                where subStr = findSubstring prefix buffer


-- | The 'findSubstring' function finds the index of the start of pat in str
findSubstring :: Eq a => [a] -> [a] -> Maybe Int
findSubstring pat str = findIndex (isPrefixOf pat) (tails str) 


-- | The 'slice" functions returns a range in a list over (from:to)
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from) (drop from xs)