import qualified Data.Attoparsec.Text as A
import Lib
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Function
import Control.Monad

---------------------------------------------------------------------------------------------------

type Pos = V2 Int
type Grid = M.Map Pos Char

parseGrid :: A.Parser Grid
parseGrid = do
    let parseLine = zip [0..] <$> A.many1 (A.char ' ' <|> A.char '.' <|> A.char '#')
    rows <- zip [0..] <$> A.many1 (parseLine <* A.endOfLine)
    (return . M.fromList) [(V2 r c, cell) | (r, row) <- rows, (c, cell) <- row, cell /= ' ']

data Dir = L | R | Move Int deriving (Show, Eq)
type Notes = [Dir]

parser :: A.Parser (Grid, Notes)
parser = do
    grid <- parseGrid <* A.endOfLine
    notes <- A.many1 ((Move <$> A.decimal) <|> (A.char 'L' $> L) <|> (A.char 'R' $> R))
    return (grid, notes)

---------------------------------------------------------------------------------------------------

data Heading = Rgt | Bot | Lft | Top deriving (Show, Eq, Enum, Ord)

turn :: Heading -> Dir -> Heading
turn h d = toEnum (mod (fromEnum h + if d == L then -1 else 1) 4)

move :: Grid -> ((Pos, Heading) -> (Pos, Heading)) -> (Pos, Heading) -> Dir -> (Pos, Heading)
move grid nextStep (pos, hd) dir = case dir of
                                    L -> (pos, turn hd L)
                                    R -> (pos, turn hd R)
                                    Move n -> step n (pos, hd)
    where step n it
              | n == 0 = it
              | let (next, _) = nextStep it in grid M.! next == '#' = it
              | otherwise = step (n - 1) (nextStep it)

delta :: Heading -> Pos
delta Lft = V2 0 (-1)
delta Rgt = V2 0 1
delta Top = V2 (-1) 0
delta Bot = V2 1 0

nextStep1 :: M.Map (Pos, Heading) Pos -> (Pos, Heading) -> (Pos, Heading)
nextStep1 jump (cur, hd) = (M.findWithDefault (cur + delta hd) (cur, hd) jump, hd)

makeJumpMap1 :: Grid -> M.Map (Pos, Heading) Pos
makeJumpMap1 g = M.fromList (do
    k <- M.keys g
    dir <- [Top, Bot, Lft, Rgt]
    guard $ not $ (k + delta dir) `M.member` g
    let oppEnd same agg = agg $ S.filter ((== same k) . same) $ M.keysSet g
    let jump = case dir of
            Lft -> oppEnd vx S.findMax
            Rgt -> oppEnd vx S.findMin
            Top -> oppEnd vy S.findMax
            Bot -> oppEnd vy S.findMin
    return ((k, dir), jump))

nextStep2 :: (Pos, Heading) -> (Pos, Heading)
nextStep2 (cur, hd) = M.findWithDefault (cur + delta hd, hd) (cur, hd) jumpMap2

-- Does not work on test grid. Intentional.
jumpMap2 :: M.Map (Pos, Heading) (Pos, Heading)
jumpMap2 = M.fromList (do
    ((a, a1, a2), (b, b1, b2)) <- [
            ((Top, V2 0 50, V2 0 99), (Lft, V2 150 0, V2 199 0)),
            ((Lft, V2 0 50, V2 49 50), (Lft, V2 149 0, V2 100 0)),
            ((Top, V2 0 100, V2 0 149), (Bot, V2 199 0, V2 199 49)),
            ((Rgt, V2 0 149, V2 49 149), (Rgt, V2 149 99, V2 100 99)),
            ((Bot, V2 49 100, V2 49 149), (Rgt, V2 50 99, V2 99 99)),
            ((Lft, V2 50 50, V2 99 50), (Top, V2 100 0, V2 100 49)),
            ((Bot, V2 149 50, V2 149 99), (Rgt, V2 150 49, V2 199 49))]
    let mkrange m n = [m + pure i * signum (n - m) | i <- [0..maximum $ abs (n - m)]]
        fwd dir1 dir2 (l1, r1) (l2, r2) = zipWith (\m n -> ((m, dir1), (n, flipD dir2))) (mkrange l1 r1) (mkrange l2 r2)
        flipD d = toEnum $ mod (fromEnum d + 2) 4
    fwd a b (a1, a2) (b1, b2) ++ fwd b a (b1, b2) (a1, a2))

main :: IO ()
main = do
    (grid, notes) <- parseInput parser
    let startCell = (S.findMin . S.filter ((== '.') . (grid M.!)) . S.filter ((== 0) . vx) . M.keysSet) grid
        walk1 = foldl (move grid (nextStep1 $ makeJumpMap1 grid)) (startCell, Rgt) notes
        solve walk = let (V2 r c, hd) = walk in (1000 * (r + 1) + 4 * (c + 1) + fromEnum hd)
    print (solve walk1)

    let walk2 = foldl (move grid nextStep2) (startCell, Rgt) notes
    print (solve walk2)
