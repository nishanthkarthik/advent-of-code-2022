import qualified Data.Attoparsec.Text as A
import Lib (parseInput)

import Data.Ix (inRange)

pairSepBy :: A.Parser a -> A.Parser s -> A.Parser (a, a)
pairSepBy parser pSkip = do
    m <- parser
    pSkip
    n <- parser
    return (m, n)

parser :: A.Parser [((Int, Int), (Int, Int))]
parser = A.sepBy (pairSepBy (pairSepBy A.decimal (A.char '-')) (A.char ',')) A.endOfLine

solve1 :: ((Int, Int), (Int, Int)) -> Bool
solve1 (l@(a, b), r@(c, d)) = (inRange l c && inRange l d) || (inRange r a && inRange r b)

solve2 :: ((Int, Int), (Int, Int)) -> Bool
solve2 (l@(a, b), r@(c, d)) = (inRange l c || inRange l d) || (inRange r a || inRange r b)

main :: IO ()
main = do
    input <- parseInput parser
    let solve f = sum . map (fromEnum . f) $ input
    print $ solve solve1
    print $ solve solve2
