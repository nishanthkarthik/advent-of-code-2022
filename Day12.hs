import qualified Data.Attoparsec.Text as A
import Lib (parseInput)

import Data.Array (Array, array, bounds, assocs, (!), (//))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Char (ord, chr)

import Control.Monad (when, unless, forM_)
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, modifySTRef)
import Data.Ix (inRange)
import Data.Bifunctor (bimap)
import Data.Maybe (fromMaybe)

type Node = (Int, Int)
type Graph = Array Node Int

findInGraph :: Int -> Graph -> Node
findInGraph x = fst . head . filter ((== x) . snd) . assocs

doWhile :: Monad m => m Bool -> m () -> m ()
doWhile cond act = cond >>= (\res -> when res (act >> doWhile cond act))

validNeighbors :: Graph -> Node -> [Node]
validNeighbors g it@(m, n) = filter (\node -> inRange (bounds g) node && (g ! node >= weight - 1)) neighbors
    where neighbors = map (bimap (m +) (n +)) [(0, 1), (0, -1), (1, 0), (-1, 0)]
          weight = g ! it

dijkstra :: Graph -> Node -> M.Map Node Int
dijkstra graph start = runST $ do
    visited <- newSTRef (S.empty :: S.Set Node)
    dist <- newSTRef (M.empty :: M.Map Node Int)
    queue <- newSTRef (S.empty :: S.Set (Int, Node))

    let distanceOf node = (\d -> if M.member node d then d M.! node else 1000) <$> readSTRef dist

    modifySTRef queue (S.insert (0, start))
    modifySTRef dist (M.insert start 0)

    doWhile (not . S.null <$> readSTRef queue) $ do
        minItem@(minDist, minNode) <- S.findMin <$> readSTRef queue
        modifySTRef queue (S.delete minItem)
        seen <- S.member minNode <$> readSTRef visited
        unless seen $ do
            let edges = validNeighbors graph minNode
            forM_ edges $ \edge -> do
                nodeDist <- distanceOf minNode
                edgeDist <- distanceOf edge
                let newDist = min edgeDist (nodeDist + 1)
                modifySTRef dist (M.insert edge newDist)
                modifySTRef queue (S.insert (newDist, edge))
            modifySTRef visited (S.insert minNode)

    readSTRef dist

infixl 6 &&&
(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) f g a = (f a, g a)

main :: IO ()
main = do
    arr <- parseInput (A.sepBy1 (A.many1 A.letter) A.endOfLine)
    let items = concat $ zipWith (\a b -> map (\(i, e) -> ((a, i), ord e)) b) [0..] (map (zip [0..]) arr)
        (m, n) = (length &&& length . head) arr
        input = array ((0, 0), (m - 1, n - 1)) items
        (start, end) = (findInGraph (ord 'S') &&& findInGraph (ord 'E')) input
        graph = input // [(start, ord 'a'), (end, ord 'z')]
        dists = dijkstra graph end
    print (dists M.! start)
    print $ (minimum . map (fromMaybe maxBound . (`M.lookup` dists) . fst) . filter ((== ord 'a') . snd) . assocs) graph
