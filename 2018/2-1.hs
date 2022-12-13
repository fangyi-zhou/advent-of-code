import Data.List (sort, group)

solve :: [String] -> Int
solve strs = twos * threes
  where
    count n = not . null . filter (== n) . map length . group . sort
    twos = length $ filter (count 2) strs
    threes = length $ filter (count 3) strs

main :: IO ()
main = interact $ show . solve . lines
