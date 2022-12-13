import Data.Char (isDigit)
import Data.Array

type Map = Array (Int, Int) Cell
data Cell = Zero | One | More deriving (Eq)

newMap = array idx [(i, Zero) | i <- range idx]

idx = ((0, 0), (1000, 1000))

next Zero = One
next One = More
next More = More

main :: IO ()
main = interact $ show . solve . map parse . lines

parse :: String -> (Int, Int, Int, Int)
parse line
  = (read a', read b', read c', read d')
    where
      removeHead = drop 2 $ dropWhile (/= '@') line
      (a', line') = span isDigit removeHead
      (b', line'') = span isDigit $ tail line'
      (c', line''') = span isDigit $ drop 2 line''
      d' = tail line'''

solve :: [(Int, Int, Int, Int)] -> Int
solve = count . foldl go newMap
  where
    go m (a, b, c, d)
      = m // [((x, y), next (m ! (x, y))) | x <- [a+1 .. a+c], y <- [b+1 .. b+d]]
    count m
      = length $ filter (== More) $ elems m
-- This is ugly and slow, but I cba to make it faster
