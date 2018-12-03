import Control.Monad (ap)
import Data.Char (isDigit)
import Data.Array

type Map = Array (Int, Int) Int

newMap = array idx [(i, 0) | i <- range idx]

idx = ((0, 0), (1000, 1000))

main :: IO ()
main = interact $ show . solve . zip [1..] . map parse . lines

parse :: String -> (Int, Int, Int, Int)
parse line
  = (read a', read b', read c', read d')
    where
      removeHead = drop 2 $ dropWhile (/= '@') line
      (a', line') = span isDigit removeHead
      (b', line'') = span isDigit $ tail line'
      (c', line''') = span isDigit $ drop 2 line''
      d' = tail line'''

solve :: [(Int, (Int, Int, Int, Int))] -> Int
solve inputs = head $ find (foldl go newMap inputs) inputs
  where
    go m (id, (a, b, c, d))
      = m // [((x, y), next (m ! (x, y)) id) | x <- [a+1 .. a+c], y <- [b+1 .. b+d]]
    next 0 id = id
    next _ _ = -1
    find m inputs
      = [id | (id, (a, b, c, d)) <- inputs, all (== id) [m ! (x, y) | x <- [a+1 .. a+c], y <- [b+1 .. b+d]]]
-- This is ugly and slow, but I cba to make it faster
