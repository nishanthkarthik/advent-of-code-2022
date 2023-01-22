import qualified Data.Attoparsec.Text as At
import Control.Applicative ((<|>))
import Lib (parseInput)
import qualified Data.Set as S
import Data.Ix (inRange)

import Debug.Trace

data Push = L | R deriving (Show, Eq)

parser :: At.Parser [Push]
parser = At.many1 ((At.char '<' >> return L) <|> (At.char '>' >> return R))

type Idx = (Int, Int)
type Rock = S.Set Idx

rocks :: [Rock]
rocks = map S.fromList [flat, plus, right, bar, block]
    where flat = [(0, 0), (1, 0), (2, 0), (3, 0)]
          plus = [(0, 1), (1, 0), (1, 1), (1, 2), (2, 1)]
          right = [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]
          bar = [(0, 0), (0, 1), (0, 2), (0, 3)]
          block = [(0, 0), (0, 1), (1, 0), (1, 1)]

type Grid = S.Set Idx

showGrid :: Grid -> String
showGrid grid = unlines (cells ++ ["-------"])
    where maxy = S.foldl max 0 $ S.map snd grid
          toChar idx = if S.member idx grid then '#' else '.'
          cells = [[toChar (x, y) | x <- [0..6]] | y <- [maxy+1,maxy..0]]

drawGrid :: Grid -> IO ()
drawGrid = putStrLn . showGrid

start :: Grid -> Rock -> (Grid, Rock)
start grid rock = (pruned, rshift)
    where maxy = S.foldl max (-1) $ S.map snd grid
          minx = S.foldl min maxBound $ S.map fst grid
          shift (x, y) = (x + 2, y + maxy + 4)
          rshift = S.map shift rock
          pruned = S.filter (\(x, y) -> y >= maxy - 50) grid

type Delta = Idx

collide :: (Grid, [Push]) -> Rock -> (Grid, [Push])
collide (g, []) rock = (g, [])
collide (grid, p:ps) rock = if canmove rock2 then collide (grid, ps) rock2 else (S.union grid rock1, ps)
    where shift (dx, dy) (x, y) = (dx + x, dy + y)
          shift1 = shift (if p == L then (-1, 0) else (1, 0))
          can1 = canmove (S.map shift1 rock)
          rock1 = if can1 then S.map shift1 rock else rock
          rock2 = S.map (shift (0, -1)) rock1
          canmove r = (all (inRange (0, 6)) . S.map fst) r && (all (>= 0) . S.map snd) r && not (any (`S.member` grid) r)

main :: IO ()
main = do
    input <- parseInput parser
    let pushstream = cycle input
        rocksfixed n = take n (cycle rocks)
        step (g, ps) r = let (g' ,r') = start g r in collide (g', ps) r'
        sim n = fst $ foldl step (S.empty, pushstream) (rocksfixed n)
        solve n = 1 + (S.foldl max 0 . S.map snd) (sim n)
        solve1 = 1 + (S.foldl max 0 . S.map snd) (sim 2022)
        scan1 = map ((1 +) . S.foldl max 0 . S.map snd . fst) $ scanl step (S.empty, pushstream) (cycle rocks)
    print (scan1 !! 20000)

    let n = 1000000000000
        len1 = 209
        sum1 = 348
        step2 = 1745
        sum2 = 2783
        deltas = zipWith (-) (tail scan1) scan1
        solve2 = sum1 + ((n - len1) `div` step2) * sum2 + sum (take (1 + (n - len1) `mod` step2) (drop len1 deltas))
    print solve2
