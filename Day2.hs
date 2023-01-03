import qualified Data.Attoparsec.Text as A
import Lib (parseInput)

import Data.Bifunctor (second)

data Play = Rock | Paper | Scissor deriving (Show, Enum, Eq)
data Outcome = Lose | Draw | Win deriving (Show, Enum, Eq)

gt :: Play -> Play -> Bool
gt Paper Rock = True
gt Rock Scissor = True
gt Scissor Paper = True
gt _ _ = False

parseMove :: A.Parser Play
parseMove = do
    c <- A.letter
    case c of
        'A' -> return Rock
        'B' -> return Paper
        'C' -> return Scissor
        _ -> fail "invalid character"

parseSecond :: A.Parser Int
parseSecond = do
    c <- A.letter
    case c of
         'X' -> return 0
         'Y' -> return 1
         'Z' -> return 2
         _ -> fail "unknown character"

parser :: A.Parser [(Play, Int)]
parser = A.many' $ do
    m1 <- parseMove
    A.space
    m2 <- parseSecond
    A.endOfLine
    return (m1, m2)

play1 :: (Play, Int) -> Int
play1 (them, n)
    | them == us = 3 + n + 1
    | us `gt` them = 6 + n + 1
    | otherwise = 0 + n + 1
    where us = toEnum n

play2 :: (Play, Int) -> Int
play2 (them, n) = case outcome of
                        Win -> 6 + 1 + mod (m + 1) 3
                        Draw -> 3 + 1 + m
                        Lose -> 0 + 1 + mod (m + 2) 3
    where outcome = toEnum n
          m = fromEnum them

main :: IO ()
main = do
    input <- parseInput parser
    print $ (sum . map play1) input
    print $ (sum . map play2) input
