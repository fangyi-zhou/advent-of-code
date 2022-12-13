import Data.List (sort, group, sortBy, nub)
import Data.Function (on)

main :: IO ()
main = interact $ show . solve . parse . lines

parse :: [String] -> [(Int, Int)]
parse = map $ \s -> read $ "("++ s ++ ")"

resultMap :: [(Int, Int)] -> [((Int, Int), Int)]
resultMap points
  = [((x, y), closest x y) | x <- [leftmost..rightmost], y <- [upmost..downmost]]
    where
      closest x y = case take 2 $ closest' x y of
                      [(id, dis1), (_, dis2)] ->
                        if dis1 == dis2 || elem id infinite then 0 else id
      closest' x y = sortBy (compare `on` snd) $ distances x y
      distances x y = [(id, abs (x - x') + abs (y - y')) | (id, (x', y')) <- points']
      leftmost = minimum $ map fst points
      rightmost = maximum $ map fst points
      upmost = minimum $ map snd points
      downmost = maximum $ map snd points
      points' = zip [1..] points
      infinite = nub $ [fst . head $ closest' x y | x <- [leftmost-1, rightmost+1], y <- [upmost..downmost]] ++ [fst . head $ closest' x y | x <- [leftmost .. rightmost], y <- [upmost-1, downmost+1]]

solve :: [(Int, Int)] -> Int
solve points
  = maximum $ map length . group . sort $ filter (/= 0) $ map snd $ resultMap points
