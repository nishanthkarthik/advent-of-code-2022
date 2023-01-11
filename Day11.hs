{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as A
import Lib (parseInput)

import qualified Data.IntMap.Strict as IM
import Data.Char (isDigit)
import Control.Applicative ((<|>))
import Text.Show.Functions
import Data.List (sortBy)
import qualified Data.Sequence as Seq

data Test = Test { divisor :: Int, throwTo :: (Int, Int) } deriving (Show)
data Actor = Actor { box :: [Int], operation :: Int -> Int, test :: Test, steps :: Int } deriving (Show)
type State = IM.IntMap Actor

parseExpr :: A.Parser (Int -> Int)
parseExpr = do
    let term = (A.string "old" >> pure 0) <|> A.decimal
        opCharToOp '*' = (*)
        opCharToOp '+' = (+)
        opCharToOp _ = error "Unknown operator"
    t1 <- term <* A.space
    op <- opCharToOp <$> (A.char '*' <|> A.char '+') <* A.space
    t2 <- term
    return (\old -> op (if t1 == 0 then old else t1) (if t2 == 0 then old else t2))

parser :: A.Parser State
parser = fmap IM.fromList $ flip A.sepBy1 A.endOfLine $ do
    let spaces = A.many1 A.space
        eol = A.endOfLine
    monkeyId <- A.string "Monkey " *> A.decimal <* A.string ":" <* eol
    bag <- spaces *> A.string "Starting items: " *> A.sepBy1 A.decimal (A.string ", ") <* eol
    op <- spaces *> A.string "Operation: new = " *> parseExpr
    denom <- spaces *> A.string "Test: divisible by " *> A.decimal <* eol
    monkeyYes <- spaces *> A.string "If true: throw to monkey " *> A.decimal <* eol
    monkeyNo <- spaces *> A.string "If false: throw to monkey " *> A.decimal <* eol
    return (monkeyId, Actor bag op (Test denom (monkeyYes, monkeyNo)) 0)

process :: (Int, Int) -> State -> Int -> State
process (denom, modulo) st i = if not . null . box $ cur then process (denom, modulo) newState i else st
    where cur = st IM.! i
          wrap = flip mod modulo
          worry = div (wrap $ operation cur (head . box $ cur)) denom
          dest = (if mod worry (divisor . test $ cur) == 0 then fst else snd) (throwTo . test $ cur)
          next = st IM.! dest
          toMonkey = next { box = box next ++ [worry] }
          fromMonkey = cur { box = (tail . box) cur, steps = 1 + steps cur }
          newState = IM.insert i fromMonkey (IM.insert dest toMonkey st)

solve :: Int -> Int -> State -> Int
solve denom nTurns = monkeyBusiness . (!! nTurns) . iterate doRound
    where monkeyBusiness = product . take 2 . sortBy (flip compare) . map steps . IM.elems
          doRound m = foldl (process (denom, modulo m)) m (IM.keys m)
          modulo = product . map (divisor . test) . IM.elems

main :: IO ()
main = do
    input <- parseInput parser
    print (solve 3 20 input)
    print (solve 1 10000 input)
