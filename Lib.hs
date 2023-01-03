module Lib where

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

parseInput :: A.Parser a -> IO a
parseInput parser = do
    input <- getArgs >>= TIO.readFile . head
    let result = A.eitherResult $ A.feed (A.parse parser input) mempty
    case result of
         Left l -> error l
         Right r -> return r
