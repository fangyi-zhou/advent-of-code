import Data.List (sort, group, sortBy, nub)
import Data.Function (on)

main :: IO ()
main = interact $ show . solve . parse . lines

parse :: [String] -> [(Int, Int)]
parse = map $ \s -> read $ "("++ s ++ ")"

resultMap :: [(Int, Int)] -> [((Int, Int), Int)]
resultMap points
  = [((x, y), accum x y) | x <- [leftmost..rightmost], y <- [upmost..downmost]]
    where
      accum x y = sum [abs (x - x') + abs (y - y') | (id, (x', y')) <- points']
      leftmost = minimum $ map fst points
      rightmost = maximum $ map fst points
      upmost = minimum $ map snd points
      downmost = maximum $ map snd points
      points' = zip [1..] points

solve :: [(Int, Int)] -> Int
solve points
  = findLess 10000 $ resultMap points

findLess :: Int -> [((Int, Int), Int)] -> Int
findLess threshold = length . filter (< threshold) . map snd
