import Data.Array
import Data.List (maximumBy)
import Data.Function (on)

main :: IO ()
main = print $ solve 7315

val :: Int -> Int -> Int -> Int
val x y serial
  = let rack = x + 10
        power = hundredth (rack * (rack * y + serial)) - 5
        hundredth x = (x `div` 100) `mod` 10 in
    power

solve :: Int -> (Int, Int, Int)
solve s = fst $ maximumBy (compare `on` snd) [((x, y, n), findnxn x y n) | n <- [1..300], x <- [1..300-n], y <- [1..300-n]]
  where
    findnxn x y n = sum [vals ! (x', y') | x' <- [x..x+n-1], y' <- [y..y+n-1]]
    vals = array ((1,1), (300,300)) [((x, y), val x y s) | x <- [1..300], y <- [1..300]]
