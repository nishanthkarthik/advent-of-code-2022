import qualified Data.Attoparsec.Text as At
import Lib
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Applicative
import Data.Functor
import Control.Monad
import Data.Bifunctor
import qualified Data.Array as A

type Grid = M.Map (V2 Int) [Dir]
type Dims = V2 Int

data Dir = Z_ | L | R | T | B deriving (Eq)

instance Show Dir where
    show Z_ = "."
    show L = "<"
    show R = ">"
    show T = "^"
    show B = "v"

showGrid :: Dims -> Grid -> String
showGrid (V2 maxr maxc) g = unlines [concat [draw $ V2 r c | c <- [0..maxc-1]] | r <- [0..maxr-1]]
    where draw p
            | p `M.member` g && length (g M.! p) == 1 = show $ head $ g M.! p
            | p `M.member` g = show $ length (g M.! p)
            | otherwise = "."

parser :: At.Parser (Grid, Dims)
parser = do
    let dot = At.char '.'
        hash = At.char '#'
        endrow = At.many1 hash *> dot *> At.many1 hash <* At.endOfLine
        el = At.choice [dot $> Z_, At.char '<' $> L, At.char '>' $> R, At.char '^' $> T, At.char 'v' $> B]
        row = hash *> (zip [0..] <$> At.many1 el) <* hash
        rows = At.many1 (row <* At.endOfLine)
    void endrow
    rs <- zip [0..] <$> rows
    void endrow
    let grid = M.fromList [(V2 ri ci, [c]) | (ri, r) <- rs, (ci, c) <- r, c /= Z_]
        dims = V2 (length rs) (length $ snd $ head rs)
    return (grid, dims)

delta :: Dir -> V2 Int
delta L = V2 0 (-1)
delta R = V2 0 1
delta T = V2 (-1) 0
delta B = V2 1 0
delta Z_ = V2 0 0

blizz :: Dims -> Grid -> Grid
blizz (V2 maxr maxc) g = M.fromListWith (++) pairs_
    where pairs_ = [(wrap (k + delta v), [v]) | (k, vs) <- M.assocs g, v <- vs]
          wrap (V2 r c) = V2 (mod r maxr) (mod c maxc)

step :: V2 Int -> Dims -> [Grid] -> S.Set (V2 Int) -> Int -> Int
step dest dims@(V2 maxr maxc) gs ps count
    | dest `elem` ps = count - 1
    | otherwise = step dest dims (tail gs) (S.fromList steps) (1 + count)
    where within (V2 r c) = (r == -1 && c == 0) || (r == maxr && c == maxc - 1) || (0 <= r && r < maxr && 0 <= c && c < maxc)
          steps = do
              p <- S.toList ps
              d <- map ((p +) . delta) [B, R, Z_, T, L]
              guard (within d && not (d `M.member` head gs))
              return d

main :: IO ()
main = do
    (grid, dims) <- parseInput parser
    let grids = cycle $ take (lcm (vx dims) (vy dims)) $ iterate (blizz dims) grid
    let end = dims - V2 0 1
        start = V2 (-1) 0
    let s1 = step end dims grids (S.singleton start) 0
        s2 = step start dims (drop s1 grids) (S.singleton end) s1
        s3 = step end dims (drop s2 grids) (S.singleton start) s2
    print (s1, s3)
