import Data.List (intersect)

solve :: [String] -> String
solve strs = head $ [fst . unzip $ filter (\x -> fst x == snd x) (zip x y) | x <- strs, y <- strs, offBy x y == 1]
  where
    offBy x y = length $ filter (/= EQ) $ zipWith compare x y

main :: IO ()
main = interact $ solve . lines
