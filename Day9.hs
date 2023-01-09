import qualified Data.Attoparsec.Text as A
import Lib (parseInput)

import Control.Applicative (liftA2)
import Data.List (nub)
import Debug.Trace (traceShowId)

data Coord = Coord { x :: Int, y :: Int } deriving (Show, Eq)
type Dir = Coord

instance Num Coord where
    (+) (Coord a b) (Coord c d) = Coord (a + c) (b + d)
    (*) (Coord a b) (Coord c d) = Coord (a * c) (b * d)
    (-) (Coord a b) (Coord c d) = Coord (a - c) (b - d)
    abs = liftA2 Coord (abs . x) (abs . y)
    signum = liftA2 Coord (signum . x) (signum . y)
    fromInteger = liftA2 Coord fromInteger fromInteger

parseDir :: A.Parser Dir
parseDir = do
    l <- A.letter
    case l of
         'L' -> pure $ Coord (-1) 0
         'R' -> pure $ Coord 1 0
         'U' -> pure $ Coord 0 1
         'D' -> pure $ Coord 0 (-1)
         _ -> error "unknown pattern"

parser :: A.Parser [(Dir, Int)]
parser = A.sepBy1 (liftA2 (,) parseDir (A.space *> A.decimal)) A.endOfLine

stepRel :: Coord -> Coord
stepRel c@(Coord a b)
    | abs a <= 1 && abs b <= 1 = 0
    | otherwise = signum c

generate :: [(Dir, Int)] -> [Coord]
generate = concatMap (uncurry (flip replicate))

stepMany :: Coord -> Coord -> [Coord] -> [Coord]
stepMany _ _ [] = []
stepMany h t (a:as) = stepRel (h + a - t) : stepMany (h + a) (stepRel (h + a - t) + t) as

uniqueCoords :: [Coord] -> Int
uniqueCoords = length . nub . scanl (+) 0

main :: IO ()
main = do
    input <- parseInput parser
    let solve n = uniqueCoords $ iterate (stepMany 0 0) (generate input) !! n
    print (solve 1)
    print (solve 9)
