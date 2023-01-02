import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Data.Either (fromRight)
import Data.List (sortBy)

parser :: A.Parser [[Int]]
parser = A.sepBy1 (A.sepBy1 A.decimal A.endOfLine) (A.many1 A.endOfLine)

input :: IO T.Text
input = getArgs >>= TIO.readFile . head

solve :: Int -> [[Int]] -> Int
solve n = sum . take n . sortBy (flip compare) . map sum

main :: IO ()
main = do
    contents <- input
    let a = A.eitherResult $ A.feed (A.parse parser contents) mempty
    print (solve 1 $ fromRight [] a)
    print (solve 3 $ fromRight [] a)
