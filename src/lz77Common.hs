module LZ77Common where
import qualified Data.ByteString               as B
import           Data.ByteString.Search
import           Data.Maybe

fileExtension :: String
fileExtension = ".hzip"

-- | If the character is a literal (Nothing, Character)
-- | If a prefix was found (Distance Back, Match)
type LZ77EncodingPair = (Maybe Int, B.ByteString)

-- | Takes a ByteString and returns the sequence encoded in LZ77
getLZ77Encoding :: B.ByteString -> Int -> Int -> Int -> [LZ77EncodingPair]
getLZ77Encoding byteString index bufferSize lookaheadSize =
    if index < B.length byteString
        then
            (start, match)
                : getLZ77Encoding byteString
                                  (index + B.length match)
                                  bufferSize
                                  lookaheadSize
        else []
  where
    (start, match) =
        getLongestPrefixInLookahead byteString index bufferSize lookaheadSize

-- | Helper method to get the lz77 encoding at a specific prefix and buffer
getLongestPrefixInLookahead :: B.ByteString -> Int -> Int -> Int -> LZ77EncodingPair
getLongestPrefixInLookahead byteString prefixIndex bufferSize lookaheadSize = longestPrefix buffer lookahead
  where
    buffer    = sliceByteString (max (prefixIndex - bufferSize) 0) prefixIndex byteString
    lookahead = sliceByteString prefixIndex (prefixIndex + lookaheadSize) byteString


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
findSubstring pat str = if null result then Nothing else Just $ head result
    where result = Data.ByteString.Search.indices pat str


-- | The 'sliceByteString' function returns a range in a ByteString over (from:to)
sliceByteString :: Int -> Int -> B.ByteString -> B.ByteString
sliceByteString from to xs = B.take (to - from) (B.drop from xs)

-- | The 'slice' function returns a range in a list over (from:to)
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from) (drop from xs)
