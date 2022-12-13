import Data.Array
import Data.List (maximumBy)

main :: IO ()
main = print $ solve 7315

val :: Int -> Int -> Int -> Int
val x y serial
  = let rack = x + 10
        power = hundredth (rack * (rack * y + serial)) - 5
        hundredth x = (x `div` 100) `mod` 10 in
    power

solve :: Int -> (Int, Int)
solve s = maximumBy f [(x, y) | x <- [1..300-2], y <- [1..300-2]]
  where
    f (x1, y1) (x2, y2)
      = compare (find3x3 x1 y1) (find3x3 x2 y2)
    find3x3 x y = sum [vals ! (x', y') | x' <- [x..x+2], y' <- [y..y+2]]
    vals = array ((1,1), (300,300)) [((x, y), val x y s) | x <- [1..300], y <- [1..300]]
