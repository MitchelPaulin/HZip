import Data.List
import System.IO
import System.Environment
import qualified Data.ByteString as B

main :: IO ()
main = do
    userInput <- getArgs
    bytes <- B.readFile (head userInput)
    putStrLn $ show bytes


-- the main idea of the longest prefix match is to look back in the buffer and find a reference to that string
longestPrefix :: Eq a => [a] -> [a] -> (Maybe Int, [a])
longestPrefix _ [] = (Nothing, [])
longestPrefix buffer prefix = if intersect prefix buffer == prefix then (findSubstring prefix buffer, prefix)
                                else longestPrefix buffer (init prefix)

findSubstring :: Eq a => [a] -> [a] -> Maybe Int
findSubstring pat str = findIndex (isPrefixOf pat) (tails str) 