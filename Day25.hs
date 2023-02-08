import Lib
import qualified Data.Attoparsec.Text as At
import Data.Functor
import Control.Monad

parser :: At.Parser [[Integer]]
parser = do
    let ch = At.char
        dig = At.choice [ch '=' $> -2, ch '-' $> -1, ch '0' $> 0, ch '1' $> 1, ch '2' $> 2]
    At.sepBy1 (At.many1 dig) At.endOfLine

toBase10 :: [Integer] -> Integer
toBase10 xs = sum $ zipWith (*) (map (5 ^) [0..]) (reverse xs)

toBase5 :: Integer -> [Integer]
toBase5 x
    | x < 5 = [x]
    | otherwise = mod x 5 : toBase5 (div x 5)

toSnafu :: [Integer] -> [Integer]
toSnafu [] = []
toSnafu (x:xs)
    | x <= 2 = x : toSnafu xs
    | otherwise = (x - 5) : toSnafu next
    where next = if null xs then [1] else 1 + head xs : tail xs

showSnafu :: [Integer] -> String
showSnafu = map ((['=', '-', '0', '1', '2'] !!) . fromIntegral . (2 +)) . reverse

main :: IO ()
main = do
    nums <- parseInput parser
    let total = (sum . map toBase10) nums
        solve1 = showSnafu . toSnafu . toBase5 $ total
    print solve1
