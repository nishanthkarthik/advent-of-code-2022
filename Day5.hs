{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as A
import Lib (parseInput)

import Control.Applicative ((<|>))
import Data.List (transpose)
import Data.Char (isSpace)
import Data.Array (array, (!), (//), Array, elems)

type Grid = Array Int [Char]
data Move = Move Int Int Int deriving (Show)
type Input = (Grid, [Move])

sepByPair :: A.Parser a -> A.Parser s -> A.Parser a
sepByPair parser sep = sep *> parser <* sep

parseGrid :: A.Parser Grid
parseGrid = do
    let emptyBlock = A.count 3 space >> return ' '
        letter = A.char '[' *> A.letter <* A.char ']'
        space = A.char ' '

    rows <- A.many1 $ A.sepBy (emptyBlock <|> letter) space <* A.endOfLine
    stackIdxs <- A.many1 (A.many' space *> A.decimal <* A.many' space) <* A.endOfLine
    let asStacks = map (dropWhile isSpace) . transpose

    return (array (1, length stackIdxs) (zip stackIdxs (asStacks rows)))

parseMoves :: A.Parser [Move]
parseMoves = A.many1 $ do
    a <- A.string "move " *> A.decimal
    b <- A.string " from " *> A.decimal
    c <- A.string " to " *> A.decimal <* A.endOfLine
    return (Move a b c)

parser :: A.Parser Input
parser = do
    grid <- parseGrid
    A.endOfLine
    moves <- parseMoves
    return (grid, moves)

move :: Int -> Int -> Int -> Grid -> Grid
move batch from to grid = grid // [(from, remnant), (to, taken ++ grid ! to)]
    where (taken, remnant) = splitAt batch (grid ! from)

moveN :: Grid -> Move -> Grid
moveN grid (Move times from to) = iterate (move 1 from to) grid !! times

move1 :: Grid -> Move -> Grid
move1 grid (Move times from to) = move times from to grid

main :: IO ()
main = do
    (grid, moves) <- parseInput parser
    let topCrates strategy = map head $ elems $ foldl strategy grid moves
    print $ topCrates moveN
    print $ topCrates move1
