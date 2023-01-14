{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as A
import Lib (parseInput)

import qualified Data.IntSet as S
import Data.Ix (inRange)
import Control.Applicative ((<|>))
import Data.Bits

type Idx = Int

encode:: (Int, Int) -> Idx
encode (l, r) = unsafeShiftL (trunc l) 16 .|. trunc r
    where trunc = (65535 .&.)

decode :: Idx -> (Int, Int)
decode i = (unsafeShiftR i 16 .&. 65535, i .&. 65535)

parsePair :: A.Parser Idx
parsePair = do
    a <- A.decimal <* A.char ','
    b <- A.decimal
    return $ encode (a, b)

parser :: A.Parser [[Idx]]
parser = A.sepBy1 (A.sepBy1 parsePair (A.string " -> ")) A.endOfLine

type Grid = S.IntSet

lineFrom :: (Idx, Idx) -> [Idx]
lineFrom (l, r)
    | l <= r = [encode (i1, i2) | i1 <- [a..c], i2 <- [b..d]]
    | otherwise = lineFrom (r, l)
    where (a, b) = decode l
          (c, d) = decode r

mkGrid :: [[Idx]] -> Grid
mkGrid = S.fromList . concatMap lineFrom . concatMap pairs
    where pairs xs = zip xs (tail xs)

data FallMode = Moving | Done | Rejected deriving (Show, Eq)

data FallState = FallState { idx :: Idx, fallMode :: FallMode} deriving (Show, Eq)

type RejectFilter = FallState -> FallState

fallStep :: RejectFilter -> Grid -> FallState -> FallState
fallStep reject grid st@(FallState idx mode)
    | mode == Moving = if fallMode stop /= Moving then stop else fallStep reject grid next
    | mode == Done = stop
    | otherwise = st
    where (i, j) = decode idx
          ways = map encode [(i, j + 1), (i - 1, j + 1), (i + 1, j + 1)]
          opts = filter (not . flip S.member grid) ways
          next = if null opts then FallState idx Done else FallState (head opts) Moving
          stop = reject st

fall :: RejectFilter -> Grid -> (Grid, Bool)
fall reject grid = (if mode == Done then S.insert idx grid else grid, mode == Done && not (S.member idx grid))
    where spout = encode (500, 0)
          start = FallState spout Moving
          stream = iterate (fallStep reject grid) start
          FallState idx mode = head $ dropWhile (\(FallState _ mode) -> mode /= Done && mode /= Rejected) stream

simulate :: RejectFilter -> Grid -> Grid
simulate reject grid = if not continue then grid else simulate reject next
    where (next, continue) = fall reject grid

main :: IO ()
main = do
    input <- parseInput parser
    let grid = mkGrid input
        bottom = (maximum . map (snd . decode) . S.toList) grid

    let reject1 st@(FallState idx mode) = st { fallMode = if mode == Moving && snd (decode idx) == bottom then Rejected else mode }
    print (S.size (simulate reject1 grid) - S.size grid)

    let reject2 st@(FallState idx mode) = st { fallMode = if mode == Moving then (if snd (decode idx) == bottom + 1 then Done else Moving) else mode }
    print (S.size (simulate reject2 grid) - S.size grid)
