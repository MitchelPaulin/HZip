import Data.List
import System.IO
import System.Environment
import qualified Data.ByteString as B

bufferSize = 10
lookaheadSize = 4

testString = "AABCBBABC"

main :: IO ()
main = do
    userInput <- getArgs
    bytes <- readFile (head userInput)
    putStrLn $ show $ map (getLZ77Encoding bytes) [0..((length bytes - 1))]

getLZ77Encoding :: Eq a => [a] -> Int -> (Maybe Int, [a])
getLZ77Encoding byteString prefixIndex = (longestPrefix buffer lookahead)
                                                where buffer = slice (max (prefixIndex - bufferSize) 0) prefixIndex byteString
                                                      lookahead = slice prefixIndex (prefixIndex + lookaheadSize) byteString

-- | The 'longestPrefix' function finds the longest prefix of 'prefix' that exists in the buffer, return Nothing if no such prefix exists
longestPrefix :: Eq a => [a] -> [a] -> (Maybe Int, [a])
longestPrefix _ [] = (Nothing, [])
longestPrefix _ [x] = (Nothing, [x])
longestPrefix buffer prefix = if findSubstring prefix buffer /= Nothing then (findSubstring prefix buffer, prefix)
                                else longestPrefix buffer (init prefix)

-- | The 'findSubstring' function finds the index of the start of pat in str
findSubstring :: Eq a => [a] -> [a] -> Maybe Int
findSubstring pat str = findIndex (isPrefixOf pat) (tails str) 

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from) (drop from xs)