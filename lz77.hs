import Data.List
import Data.Maybe
import System.IO
import System.Environment
import qualified Data.ByteString as B

bufferSize = 1000
lookaheadSize = 100

main :: IO ()
main = do
    userInput <- getArgs
    -- plain text encoding
    -- bytes <- readFile (head userInput)
    -- print (getLZ77Encoding bytes 0)
    -- byte based encoding
    bytes <- B.readFile (head userInput)
    print (getLZ77Encoding (B.unpack bytes) 0)


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