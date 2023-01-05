import qualified Data.Attoparsec.Text as A
import Lib (parseInput)

import Data.Char (isAlpha)
import Data.List (nub, tails)
import qualified Data.Text as T

solve :: Int -> T.Text -> Int
solve n = (+ n) . length . takeWhile ((/= n) . length) . map (nub . take n) . tails . T.unpack

main :: IO ()
main = do
    input <- parseInput (A.takeWhile isAlpha <* A.endOfLine)
    print $ solve 4 input
    print $ solve 14 input
