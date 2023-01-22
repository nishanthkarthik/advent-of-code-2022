import qualified Data.Attoparsec.Text as At
import Text.Printf (printf)
import Lib (parseInput)
import Data.Ix (inRange)
import qualified Data.Set as S

import Control.Monad.ST
import Data.STRef
import Control.Monad

type V3 = (Int, Int, Int)

parser :: At.Parser [V3]
parser = flip At.sepBy1 At.endOfLine $ do
    a <- At.decimal <* At.char ','
    b <- At.decimal <* At.char ','
    c <- At.decimal
    return (a, b, c)

neighbors :: V3 -> [V3]
neighbors (x, y, z) = map addcur offsets
    where offsets = concat [[(a, 0, 0), (0, a, 0), (0, 0, a)] | a <- [-1, 1]]
          addcur (dx, dy, dz) = (x + dx, y + dy, z + dz)

area :: S.Set V3 -> V3 -> Int
area cubes = length . filter (not . (`S.member` cubes)) . neighbors

dfs :: S.Set V3 -> Int
dfs cubes = recurse
    where lb f = S.foldl (\ac it -> min ac (f it)) maxBound cubes - 2
          ub f = S.foldl (\ac it -> max ac (f it)) minBound cubes + 2
          d1 (x, _, _) = x
          d2 (_, y, _) = y
          d3 (_, _, z) = z
          bounds = ((lb d1, lb d2, lb d3), (ub d1, ub d2, ub d3))
          recurse = runST $ do
              visited <- newSTRef S.empty

              let step node = do
                      visited_ <- readSTRef visited
                      
                      if S.member node visited_ || S.member node cubes || not (inRange bounds node)
                         then return 0
                         else do
                            modifySTRef visited (S.insert node)
                            let neighs = neighbors node
                                nwalls = length $ filter (`S.member` cubes) neighs
                            (nwalls +) . sum <$> mapM step neighs

              step (fst bounds)

main :: IO ()
main = do
    a <- S.fromList <$> parseInput parser
    let solve1 = (sum . map (area a) . S.toList) a
    print solve1
    print (dfs a)
