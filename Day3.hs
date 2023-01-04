import qualified Data.Attoparsec.Text as A
import Lib (parseInput)

import qualified Data.Text as T
import Data.Char (isAlpha, ord, isAsciiLower, isAsciiUpper)
import Data.Int (Int64)
import Data.Bits
import Data.List.Split (chunksOf)

parser :: A.Parser [T.Text]
parser = A.sepBy1 (A.takeWhile1 isAlpha) A.endOfLine

toNum :: T.Text -> Int64
toNum = T.foldl (\ac it -> setBit ac (ord' it)) 0
    where ord' c
            | isAsciiLower c = fromIntegral $ 0 + (ord c - ord 'a')
            | isAsciiUpper c = fromIntegral $ 26 + (ord c - ord 'A')
            | otherwise = undefined

solve1 :: T.Text -> Int
solve1 t = 1 + countTrailingZeros (toNum a .&. toNum b)
    where [a, b] = T.chunksOf (T.length t `div` 2) t

solve2 :: [T.Text] -> Int
solve2 = sum . map ((1 +) . countTrailingZeros . foldl1 (.&.) . map toNum) . chunksOf 3

main :: IO ()
main = do
    input <- parseInput parser
    print $ sum $ map solve1 input
    print $ solve2 input
