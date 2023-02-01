{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as At
import Lib
import Data.Char (isLower)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Applicative

data Op = Add | Sub | Mul | Div deriving (Eq, Show)
data Expr = Val Double | Var T.Text | Node Op Expr Expr deriving (Eq, Show)

type Input = M.Map T.Text Expr

parseOp :: At.Parser Op
parseOp = do
    c <- At.anyChar
    case c of
         '+' -> return Add
         '-' -> return Sub
         '*' -> return Mul
         '/' -> return Div
         _ -> error "unknown"

parser :: At.Parser (T.Text, Expr)
parser = do
    let token = At.takeWhile1 isLower
        num = Val . fromIntegral <$> At.signed At.decimal
        expr = do
            a <- token <* At.string " "
            op <- parseOp <* At.string " "
            Node op (Var a) <$> (Var <$> token)
    lhs <- token <* At.string ": "
    rhs <- num <|> expr
    return (lhs, rhs)

solve1 :: Input -> T.Text -> Integer
solve1 input start = round $ go (Var start)
    where go :: Expr -> Double
          go (Val a) = a
          go (Var a) = go (input M.! a)
          go (Node Add a b) = go a + go b
          go (Node Sub a b) = go a - go b
          go (Node Mul a b) = go a * go b
          go (Node Div a b) = go a / go b

solve2 :: Input -> Integer
solve2 input = round $ binSearch lb rb
    where (Node _ (Var lhs) (Var rhs)) = input M.! "root"
          solveWith humn = let i' = M.insert "humn" (Val humn) input in (solve1 i' lhs - solve1 i' rhs)
          (lb, rb) = (fromIntegral (minBound :: Int), fromIntegral (maxBound :: Int))
          comp = if solveWith lb < solveWith rb then (<) else (>)
          binSearch l r
              | solveWith mid == 0 = mid
              | solveWith mid `comp` 0 = binSearch (mid + 1) r
              | otherwise = binSearch l (mid - 1)
              where mid = (l + r) / 2

main :: IO ()
main = do
    input <- M.fromList <$> parseInput (At.sepBy1 parser At.endOfLine)
    print (solve1 input "root")
    print (solve2 input)
