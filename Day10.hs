{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as A
import Lib (parseInput)

import Control.Applicative ((<|>))
import Data.Array (array, Array, (//), bounds, elems)
import Data.List.Split (chunksOf)

data Instr = Nop | Add Int deriving (Show, Eq)

parser :: A.Parser [Instr]
parser = A.sepBy1 (parseAdd <|> parseNop) A.endOfLine
    where parseAdd = Add <$> (A.string "addx" *> A.space *> A.signed A.decimal)
          parseNop = A.string "noop" >> return Nop

stream :: (Int, [Int]) -> Instr -> (Int, [Int])
stream (reg, signals) instr = case instr of
                                   Nop -> (reg, reg : signals)
                                   Add n -> (reg + n, reg : reg : signals)

everyNth :: Int -> [a] -> [a]
everyNth n [] = []
everyNth n xs = head xs : everyNth n (drop n xs)

type CRT = Array Int Char

crtDims = (6, 40)

draw :: (Int, CRT) -> Int -> (Int, CRT)
draw (cur, crt) reg = (nextcrt, crt // [(cur, '#') | abs (mod cur (snd crtDims) - reg) <= 1])
    where nextcrt = mod (cur + 1) (uncurry (*) crtDims)

main :: IO ()
main = do
    input <- parseInput parser
    let (reg, signals) = foldl stream (1, []) input
        keys = [20,60..]
        vals = (everyNth 40 . drop 19 . reverse) signals
        solve1 = sum $ zipWith (*) keys vals
        crtMax = uncurry (*) crtDims - 1
        crt = array (0, crtMax) [(i, '.') | i <- [0..crtMax]]
        solve2 = foldl draw (0, crt) (reverse signals)
    print solve1
    mapM_ putStrLn $ chunksOf (snd crtDims) (elems (snd solve2))
