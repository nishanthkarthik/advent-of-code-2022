{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.Attoparsec.Text as A
import Lib (parseInput)

import Data.Text (Text)
import Data.Char (isSpace)
import Data.List (sort)
import Control.Applicative ((<|>))
import qualified Data.Map.Strict as M

data Entry = Dir Text | File Text Integer deriving (Show, Eq, Ord)
data Command = Cd Text | Ls [Entry] deriving (Show)

type Cwd = [Entry]
type Fs = M.Map (Entry, [Entry]) [Entry]

parseCd :: A.Parser Command
parseCd = Cd <$> (A.string "$ cd " *> A.takeTill isSpace <* A.endOfLine)

parseEntries :: A.Parser [Entry]
parseEntries = do
    let dir = Dir <$> (A.string "dir " *> A.takeTill isSpace)
        file = do
            size <- A.decimal <* A.space
            name <- A.takeTill isSpace
            return (File name size)
    A.many' ((dir <|> file) <* A.endOfLine)

parseLs :: A.Parser Command
parseLs = (A.string "$ ls" *> A.endOfLine) *> (Ls <$> parseEntries)

parser :: A.Parser [Command]
parser = A.many' (parseLs <|> parseCd)


reconstruct :: (Fs, Cwd) -> Command -> (Fs, Cwd)
reconstruct (fs, cwd) cmd = case cmd of
                                 Cd dir -> case dir of
                                                ".." -> (fs, tail cwd)
                                                _ -> (fs, Dir dir : cwd)
                                 Ls entries -> (M.insert (head cwd, if null cwd then [] else tail cwd) entries fs, cwd)

du :: Fs -> (Entry, Cwd) -> Integer
du fs (entry, cwd) = case entry of
                   File _ size -> size
                   Dir _ -> sum (map (du fs . (, entry : cwd)) (fs M.! (entry, cwd)))

main :: IO ()
main = do
    input <- parseInput parser
    let (fs, cwd) = foldl reconstruct (M.empty, []) input
    let dus = map (du fs) $ M.keys fs
        df = 70000000 - head dus
        solve1 = sum $ filter (<= 100000) dus
        solve2 = head $ dropWhile ((< 30000000) . (df +)) (sort dus)
    print solve1
    print solve2
