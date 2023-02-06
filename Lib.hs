module Lib where

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Text.Printf

parseInput :: A.Parser a -> IO a
parseInput parser = do
    input <- getArgs >>= TIO.readFile . head
    let result = A.eitherResult $ A.feed (A.parse parser input) mempty
    case result of
         Left l -> error l
         Right r -> return r


data V2 a = V2 { vx :: !a, vy :: !a } deriving (Eq, Ord)

instance Show a => Show (V2 a) where show v = let V2 a b = show <$> v in printf "{%s, %s}" a b

instance Functor V2 where fmap f (V2 a b) = V2 (f a) (f b)

instance Applicative V2 where
    pure a = V2 a a
    (<*>) (V2 fa fb) (V2 a b) = V2 (fa a) (fb b)

instance Foldable V2 where foldr f i (V2 x y) = foldr f i [x, y]

instance Num a => Num (V2 a) where
    (+) va vb = (+) <$> va <*> vb
    (*) va vb = (*) <$> va <*> vb
    abs = fmap abs
    signum = fmap signum
    fromInteger i = pure (fromInteger i)
    negate = fmap negate
