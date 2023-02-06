import Lib
import qualified Data.Attoparsec.Text as At
import qualified Data.Set as S
import qualified Data.Map.Lazy as M
import Data.Functor (($>))
import Data.List (transpose)
import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Control.Monad (guard)

type Grid = S.Set (V2 Int)

parser :: At.Parser Grid
parser = do
    let row = zip [0..] <$> At.many1 (At.char '.' $> False <|> At.char '#' $> True)
    rs <- zip [0..] <$> At.sepBy1 row At.endOfLine
    return $ S.fromList [V2 ri ci | (ri, cs) <- rs, (ci, c) <- cs, c]

decide :: Int -> Grid -> V2 Int -> V2 Int
decide offset g a
    | empty [V2 i j | i <- [-1..1], j <- [-1..1], i /= 0 || j /= 0] = a
    | otherwise = a + if null checks then V2 0 0 else head checks
    where empty = not . any ((`S.member` g) . (a +))
          expand a = empty $ map a [-1..1]
          modes = map (first expand) [(V2 (-1), V2 (-1) 0), (V2 1, V2 1 0), (flip V2 (-1), V2 0 (-1)), (flip V2 1, V2 0 1)]
          checks = map snd $ filter fst $ take 4 $ drop (mod offset 4) $ cycle modes

playRound :: Grid -> Int -> Grid
playRound grid offset = S.fromList validNexts
    where step = decide offset grid
          olds = S.toList grid
          counts = foldl addCount M.empty (map step olds)
          addCount m a = M.insertWith (+) a 1 m
          validNexts = map (\a -> if counts M.! step a == 1 then step a else a) olds

emptyTiles :: Grid -> Int
emptyTiles g = extent vx * extent vy - S.size g
    where extent = (1 +) . ((-) <$> S.findMax <*> S.findMin) . flip S.map g

main :: IO ()
main = do
    grid <- parseInput parser
    let solve = scanl playRound grid [0..]
        solve1 = solve !! 10
        solve2 = do
            (i, a, b) <- zip3 [1..] solve $ tail solve
            guard (a == b)
            return i
    print (emptyTiles solve1)
    print (head solve2)
