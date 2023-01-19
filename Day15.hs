{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as A
import Data.Ix (inRange)
import Data.List (sortBy)
import Data.List.Extra (nubOrd)
import qualified Data.Map.Lazy as M
import Data.Maybe (mapMaybe)
import Lib (parseInput)

type V2 = (Int, Int)

type Input = M.Map V2 V2

manhattan :: V2 -> V2 -> Int
manhattan (a, b) (c, d) = abs (a - c) + abs (b - d)

parser :: A.Parser Input
parser =
  M.fromList
    <$> A.sepBy1 (do
            let coord = A.signed A.decimal
            sx <- A.string "Sensor at x=" *> coord
            sy <- A.string ", y=" *> coord
            bx <- A.string ": closest beacon is at x=" *> coord
            by <- A.string ", y=" *> coord
            return ((sx, sy), (bx, by))
        ) A.endOfLine

intersect :: Int -> (V2, V2) -> Maybe V2
intersect yref (s@(x, y), b) = if dist >= 0 then Just bound else Nothing
    where dist = manhattan s b - abs (yref - y)
          bound = (x - dist, x + dist)

overlap :: V2 -> V2 -> [V2]
overlap l@(a, b) r@(c, d)
    | inRange l c || inRange l d = [(minimum [a, b, c, d], maximum [a, b, c, d])]
    | otherwise = [l, r]

data Tag = L Int | R Int deriving (Show, Eq, Ord)

pairs :: [a] -> [(a, a)]
pairs xs
    | length xs <= 1 = []
    | otherwise = (head xs, xs !! 1) : pairs (drop 2 xs)

fuseOverlaps :: [V2] -> [V2]
fuseOverlaps v = pairs markers
    where tags = sortBy compTag $ concatMap toTag v
          prefixSums = scanl (+) 0 (map marker tags)
          triplets = zip3 prefixSums (tail prefixSums) tags
          boundaries = filter (\(a, b, _) -> (a, b) == (0, 1) || (a, b) == (1, 0)) triplets
          markers = map (fromTag . \(_, _, a) -> a) boundaries
          toTag (a, b) = [L a, R b]
          fromTag (L a) = a
          fromTag (R b) = b
          marker (L _) = 1
          marker (R _) = -1
          compTag a b = if numOrd == EQ then compare a b else numOrd
            where numOrd = compare (fromTag a) (fromTag b)

pairsOf :: [a] -> [a] -> [(a, a)]
pairsOf x y = [(a, b) | a <- x, b <- y]

main :: IO ()
main = do
    input <- parseInput parser
    let yref = 2000000
    let intervals y = (fuseOverlaps . mapMaybe (intersect y) . M.toList) input
        beacons = nubOrd (M.elems input)
        solve1 = map (\(a, b) -> b - a + 1) $ intervals yref
        beaconOverlaps = [fromEnum $ inRange r m | r <- intervals yref, (m, n) <- beacons, n == yref]
    print (sum solve1 - sum beaconOverlaps)

    let lines1c = [y - x + manhattan s b + c | (s@(x, y), b) <- M.assocs input, c <- [-1, 1]]
        lines2c = [y + x + manhattan s b + c | (s@(x, y), b) <- M.assocs input, c <- [-1, 1]]
        combos = nubOrd $ map ((`div` 2) . uncurry (+)) $ pairsOf lines1c lines2c
        -- if a window is open between two intervals, there should be exactly one element in between
        xions = [(y, intervals y) | y <- combos, length (intervals y) == 2, 2 + snd (head $ intervals y) == fst (intervals y !! 1)]
        (y, [l, r]) = head xions
        solve2 = y + 2000000 * (fst r + snd l)
    print solve2
