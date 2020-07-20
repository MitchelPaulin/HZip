module BitsCommon where
import           GHC.Word

type Bit = Bool
type Bits = [Bit]

byteSize :: Int
byteSize = 8

-- | The 'padListTo8' function takes a list of Bits and pads it with false
padListTo8 :: [Bits] -> [Bits]
padListTo8 x = init x ++ [reverse $ padWithZeros 8 (reverse $ last x)]

-- | The 'wordToBits' function converts a word8 into a sequence of Trues and Falses representing high and low bits
wordToBits :: GHC.Word.Word8 -> Bits
wordToBits word = padWithZeros 8 res
                  where res = intToBits (fromIntegral word)

-- | The 'bitsToWord' function takes a list of bits and packs them into an 8 bits word
bitsToWord :: Bits -> GHC.Word.Word8
bitsToWord bits = fromIntegral $ bitsToInt bits

-- | The 'boolToBit' function converts a single bit to a boolean
bitsToInt :: Bits -> Int
bitsToInt b = foldr (\x y -> fromEnum x + 2*y) 0 (reverse b)

-- | The 'intToBits' function takes and integer and returns a binary representation in the form of [Bool]
intToBits :: Int -> Bits
intToBits 0 = [False]
intToBits n = map (> 0) (go n [])
    where go 0 r = r
          go k rs = go (div k 2) (mod k 2:rs)

-- | The 'padWithZeros' function pads Bits with zeros
padWithZeros :: Int -> Bits -> Bits 
padWithZeros n bits = if length bits < n then replicate (n - length bits) False ++ bits
                        else bits