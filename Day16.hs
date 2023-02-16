{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as At
import qualified Data.Text as T
import Control.Applicative ((<|>))
import Lib (parseInput)

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.Int (Int64)
import qualified Data.Bits as B
import qualified Data.IntSet as IS

import Control.Monad
import Control.Monad.ST (runST)
import Data.STRef
import qualified Data.Array as Ar

type Input = [(T.Text, (Int, [T.Text]))]
type Graph = IM.IntMap (Int, [Int])

parser :: At.Parser Input
parser = flip At.sepBy1 At.endOfLine $ do
    let node = At.count 2 At.letter
    f <- At.string "Valve " *> (T.pack <$> node)
    r <- At.string " has flow rate=" *> At.decimal
    At.string "; tunnels lead to valves " <|> At.string "; tunnel leads to valve "
    ts <- map T.pack <$> At.sepBy1 node (At.string ", ")
    return (f, (r, ts))

toGraph :: Input -> Graph
toGraph xs = IM.fromList (map fn xs)
    where idxs = M.fromList $ zip (map fst xs) [0..]
          fn (f, (r, ts)) = (idxs M.! f, (r, map (idxs M.!) ts))

type Dist = Ar.Array (Int, Int) Int

floydW :: Graph -> Dist
floydW g = runST $ do
    let n = IM.size g
        idxs = IM.keys g
    dist <- newSTRef (Ar.array ((0, 0), (n - 1, n - 1)) [((i, j), 100000 :: Int) | i <- idxs, j <- idxs])

    let edgeWeights = [((u, v), 1) | u <- IM.keys g, v <- snd $ g IM.! u]
        selfDists = [((u, u), 0) | u <- IM.keys g]
    modifySTRef dist (Ar.// (edgeWeights ++ selfDists))

    forM_ [(k, i, j) | k <- idxs, i <- idxs, j <- idxs] $ \(k, i, j) -> do
        (ij, ik, kj) <- ((,,) <$> (Ar.! (i, j)) <*> (Ar.! (i, k)) <*> (Ar.! (k, j))) <$> readSTRef dist
        when (ij > ik + kj) (modifySTRef dist (Ar.// [((i, j), ik + kj)]))

    readSTRef dist

type Flow = Ar.Array Int Int

-- search flowtable distances both timeleft current nodesleft
search :: Flow -> Dist -> Bool -> Int -> Int -> Int64 -> Int
search f d b = impl
    where impl t u r = foldl max 0 (elephant : human)
            where human = [rec v r | v <- [0..63], B.testBit r v, d Ar.! (u, v) < t, f Ar.! v > 0]
                  elephant = if b then search f d False 26 0 r else 0
                  trem v = t - d Ar.! (u, v) - 1
                  rec v r' = f Ar.! v * trem v + search f d b (trem v) v (B.clearBit r' v)

main :: IO ()
main = do
    graph <- toGraph <$> parseInput parser
    let shortest = floydW graph
        n = IM.size graph
        flows = Ar.array (0, n - 1) $ IM.assocs (IM.map fst graph)
        solve b t = search flows shortest b t 0 (B.shiftL 1 n - 1)
    print (solve False 30)
    print (solve True 26)
    return ()
