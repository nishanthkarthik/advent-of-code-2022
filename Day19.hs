{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import Lib

import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Applicative ((<|>))
import Data.Char (isLower)
import Data.Maybe (fromMaybe)
import Control.Parallel.Strategies
import Text.Printf (printf)
import Data.List (foldl')
import Data.MemoTrie (memo3)

data V4 a = V4 { vx :: !a, vy :: !a, vz :: !a, vw :: !a } deriving (Eq)

instance Show a => Show (V4 a) where
    show v = let V4 a b c d = show <$> v in printf "{%s, %s, %s, %s}" a b c d

instance Functor V4 where
    fmap f (V4 a b c d) = V4 (f a) (f b) (f c) (f d)

instance Applicative V4 where
    pure a = V4 a a a a
    (<*>) (V4 fa fb fc fd) (V4 a b c d) = V4 (fa a) (fb b) (fc c) (fd d)

instance Foldable V4 where
    foldr f i (V4 x y z w) = foldr f i [x, y, z, w]

instance Num a => Num (V4 a) where
    (+) va vb = (+) <$> va <*> vb
    (*) va vb = (*) <$> va <*> vb
    abs = fmap abs
    signum = fmap signum
    fromInteger i = pure (fromInteger i)
    negate = fmap negate

type Blueprint = M.Map T.Text [(T.Text, Int)]

parseBlueprint :: At.Parser (Int, Blueprint)
parseBlueprint = do
    let spaces = At.many1 (At.space <|> (At.endOfLine >> pure ' '))
        name = At.takeWhile isLower
        comp = do
            n <- At.decimal
            t <- At.space *> name
            return (t, n)
        parseCost = do
            what <- At.string "Each " *> name <* At.string " robot costs "
            comps <- At.sepBy1 comp (At.string " and ") <* At.char '.'
            return (what, comps)
    i <- At.string "Blueprint " *> At.decimal <* (At.char ':' <* spaces)
    costs <- M.fromList <$> At.sepBy1 parseCost spaces
    return (i, costs)

parser :: At.Parser (M.Map Int Blueprint)
parser = M.fromList <$> At.sepBy1 parseBlueprint (At.many1 At.endOfLine)

type Recipe = [V4 Int]

processInput :: Blueprint -> Recipe
processInput m = [fromList [cost i j | j <- [0..3]] | i <- [0..3]]
    where keys i = ["ore", "clay", "obsidian", "geode"] !! i
          cost i j = if M.member (keys i) m then fromMaybe 0 (lookup (keys j) (m M.! keys i)) else 0
          fromList xs = V4 (head xs) (xs !! 1) (xs !! 2) (xs !! 3)

type Robots = V4 Int
type Gems = V4 Int

step :: Int -> Recipe -> Robots -> Gems -> Int -> Int
step !stopLimit !recipe !robots !gems !time
    | time == stopLimit = vw gems
    | otherwise = (foldl' max minBound . map next . filter shouldMake . filter canMake) [0..3]
    where canMake i = signum (recipe !! i) * signum robots == signum (recipe !! i)
          shouldMake i 
              | i <= 2 = (fns !! i) robots <= (maximum . map (fns !! i)) recipe
              | otherwise = True
          fns = [vx, vy, vz, vw]
          minsToWait i = 1 + maximum (ceildiv <$> clipZero (recipe !! i - gems) <*> robots)
          nextGems i = gems + pure (minsToWait i) * robots - recipe !! i
          nextRobots i = robots + [V4 1 0 0 0, V4 0 1 0 0, V4 0 0 1 0, V4 0 0 0 1] !! i
          next i
              | time + minsToWait i > stopLimit = vw robots * (stopLimit - time) + vw gems
              | otherwise = step stopLimit recipe (nextRobots i) (nextGems i) (time + minsToWait i) `using` (if time >= 24 then rseq else rpar)
          ceildiv n d
              | d == 0 && n == 0 = 0
              | otherwise = div (n + d - 1) d
          clipZero v = (\i -> if i >= 0 then i else 0) <$> v

main :: IO ()
main = do
    input <- M.map processInput <$> parseInput parser
    print ("Size", M.size input)
    let solve lim i = step lim (input M.! i) (V4 1 0 0 0) (V4 0 0 0 0) 0
        solve1 = sum . parMap rpar (\i -> i * solve 24 i) . M.keys $ input
        solve2 = (product . parMap rpar (solve 32)) [1..3]
    print solve1
    print solve2
