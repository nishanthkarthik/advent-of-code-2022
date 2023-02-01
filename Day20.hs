import qualified Data.Attoparsec.Text as At
import qualified Data.Sequence as S
import Lib
import Data.Maybe (fromJust)
import Data.Bifunctor (second)

type Ring = S.Seq (Int, Int)

findIndexL :: (a -> Bool) -> S.Seq a -> Int
findIndexL predicate s = fromJust (S.findIndexL predicate s)

move :: Ring -> Int -> Ring
move ring token = ring_
    where idx = findIndexL ((== token) . fst) ring
          cur@(_, val) = S.index ring idx
          insertIdx = mod (idx + val) (S.length ring - 1)
          ring_ = S.insertAt insertIdx cur (S.deleteAt idx ring)

main :: IO()
main = do
    input <- S.fromList <$> parseInput (At.sepBy1 (At.signed At.decimal :: At.Parser Int) At.endOfLine)
    let n = S.length input
        ring = S.zip (S.fromList [0..n-1]) input
        mix r = foldl move r [0..n-1]
        solve1 r = let itemAfter i = snd $ S.index r (mod (findIndexL ((== 0) . snd) r + i) n)
                   in sum $ map itemAfter [1000, 2000, 3000]

    print (solve1 (mix ring))
    print (solve1 (iterate mix (second (811589153 *) <$> ring) !! 10))
