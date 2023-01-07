import qualified Data.Attoparsec.Text as A
import Lib (parseInput)

import Data.Array (array, Array, bounds, (!), indices)
import Data.Maybe (fromMaybe)
import Data.Ix (inRange)
import Data.List (concat, all, findIndex)
import Data.Char (digitToInt)
import Control.Applicative (liftA2)

type GridIx = (Int, Int)
type Grid = Array GridIx Int

parser :: A.Parser Grid
parser = fuse <$> A.sepBy (map digitToInt <$> A.many1 A.digit) A.endOfLine
    where fuse xs = array (idx xs) (liftA2 zip idxs concat xs)
          idxs xs = [(i, j) | i <- [0..dim xs - 1], j <- [0..dim xs - 1]]
          dim xs = length $ head xs
          idx xs = ((0, 0), (dim xs - 1, dim xs - 1))

type Step = GridIx

viewLine :: Grid -> GridIx -> Step -> [Int]
viewLine grid idx step = map (grid !) indices
    where indices = takeWhile (inRange (bounds grid)) (drop 1 $ iterate (addT step) idx)
          addT (a, b) (c, d) = (a + c, b + d)

viewLines :: Grid -> GridIx -> [[Int]]
viewLines grid idx = map (viewLine grid idx) walks
    where walks = [(0, 1), (0, -1), (1, 0), (-1, 0)]

isVisible :: Grid -> GridIx -> Bool
isVisible grid ix = any (all (< grid ! ix)) (viewLines grid ix)

solve1 :: Grid -> Int
solve1 grid = sum $ map (fromEnum . isVisible grid) (indices grid)

sameHeightOrTaller :: Int -> [Int] -> [Int]
sameHeightOrTaller _ [] = []
sameHeightOrTaller ref (x : xs)
    | x >= ref = [x]
    | otherwise = x : sameHeightOrTaller ref xs

solve2 :: Grid -> Int
solve2 grid = maximum (map score (indices grid))
    where score ix = product $ map (length . sameHeightOrTaller (grid ! ix)) (viewLines grid ix)

main :: IO ()
main = do
    input <- parseInput parser
    print $ solve1 input
    print $ solve2 input
