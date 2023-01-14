import qualified Data.Attoparsec.Text as A
import Lib (parseInput)

import Control.Applicative ((<|>), liftA2)
import Data.List (sortBy, intercalate, findIndices)
import Data.Maybe (fromJust)

data Packet = Leaf Int | List [Packet] deriving (Eq)

instance Show Packet where
    show (Leaf a) = show a
    show (List as) = "[" ++ intercalate "," (map show as) ++ "]"

parsePacket :: A.Parser Packet
parsePacket = List <$> (A.char '[' *> A.sepBy (Leaf <$> A.decimal <|> parsePacket) (A.char ',') <* A.char ']')

parser :: A.Parser [(Packet, Packet)]
parser = flip A.sepBy A.endOfLine $ do
    p1 <- parsePacket <* A.endOfLine
    p2 <- parsePacket <* A.endOfLine
    return (p1, p2)

pktComp :: Packet -> Packet -> Maybe Bool
pktComp (List []) (List []) = Nothing
pktComp (List []) _ = Just True
pktComp _ (List []) = Just False
pktComp (Leaf l) (Leaf r) = if l == r then Nothing else Just (l < r)
pktComp (List (l : ls)) (List (r : rs)) = pktComp l r <|> pktComp (List ls) (List rs)
pktComp (Leaf l) (List rs) = pktComp (List [Leaf l]) (List rs)
pktComp (List ls) (Leaf r) = pktComp (List ls) (List [Leaf r])

main :: IO ()
main = do
    input <- parseInput parser
    let solve1 = (sum . map fst . filter ((== Just True) . snd) . zip [1..] . map (uncurry pktComp)) input
    print solve1
    let input2 = concatMap (\(a, b) -> [a, b]) input
        partn i = length . filter ((== Just False) . pktComp (List [List [Leaf i]]))
        solve2 = (1 + partn 2 input2) * (2 + partn 6 input2)
    print solve2
