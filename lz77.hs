import Data.List
import System.IO
import System.Environment
import qualified Data.ByteString as B

bufferSize = 10
lookaheadSize = 4

main :: IO ()
main = do
    userInput <- getArgs
    bytes <- readFile (head userInput)
    putStrLn $ show $ encodeText bytes 0


-- | Takes a string of text and returns the sequence encoded in LZ77, the end of the sequence is marked with a (Nothing, [])
encodeText :: Eq a => [a] -> Int -> [(Maybe Int, [a])]
encodeText byteString index = if index < length byteString 
                                then (start, match) : encodeText byteString (index + (length match))
                                else [(Nothing, [])]
                        where (start, match) = getLZ77Encoding byteString index

-- | Helper method to get the lz77 encoding at a specific prefix
getLZ77Encoding :: Eq a => [a] -> Int -> (Maybe Int, [a])
getLZ77Encoding byteString prefixIndex = longestPrefix buffer lookahead
                                                where buffer = slice (max (prefixIndex - bufferSize) 0) prefixIndex byteString
                                                      lookahead = slice prefixIndex (prefixIndex + lookaheadSize) byteString


-- | The 'longestPrefix' function finds the longest prefix of 'prefix' that exists in the buffer, return Nothing if no such prefix exists
longestPrefix :: Eq a => [a] -> [a] -> (Maybe Int, [a])
longestPrefix _ [] = (Nothing, [])
longestPrefix _ [x] = (Nothing, [x])
longestPrefix buffer prefix = if  subStr /= Nothing then (subStr, prefix)
                                else longestPrefix buffer (init prefix)
                                where subStr = findSubstring prefix buffer


-- | The 'findSubstring' function finds the index of the start of pat in str
findSubstring :: Eq a => [a] -> [a] -> Maybe Int
findSubstring pat str = findIndex (isPrefixOf pat) (tails str) 


-- | The 'slice" functions returns a range in a list over (from:to)
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from) (drop from xs)