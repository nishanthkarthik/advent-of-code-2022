import qualified Data.Attoparsec.Text as A
import Lib (parseInput)

import Data.List (sortBy)

parser :: A.Parser [[Int]]
parser = A.many' $ do
    ds <- A.many' $ do
        d <- A.decimal
        A.endOfLine
        return d
    A.endOfLine
    return ds

solve :: Int -> [[Int]] -> Int
solve n = sum . take n . sortBy (flip compare) . map sum

main :: IO ()
main = do
    input <- parseInput parser
    print (solve 1 input)
    print (solve 3 input)
