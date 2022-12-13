import Data.Char (isDigit)
import Data.Function (on)
import Data.List (maximumBy)

main :: IO ()
main = interact $ show . solve . parse

parse :: String -> [((Int, Int, Int), Int)]
parse = foldl go [] . lines
  where
    go acc line = ((x, y, z), r) : acc
      where
        (_, x') = splitAt 5 line
        (x'', x''') = break (== ',') x'
        x = read x''
        (y', y'') = break (== ',') $ tail x'''
        y = read y'
        (z', z'') = break (== '>') $ tail y''
        z = read z'
        (_, r') = break (== '=') z''
        r = read $ tail r'

solve :: [((Int, Int, Int), Int)] -> Int
solve pts = length $ filter (inRange largest) pts
  where
    inRange ((x, y, z), r) ((x', y', z'), _)
      = abs (x - x') + abs (y - y') + abs (z - z') <= r
    largest = maximumBy (compare `on` snd) pts
