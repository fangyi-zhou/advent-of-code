import Data.Char (isDigit)
import qualified Data.Map as M

main :: IO ()
main = interact $ show . solve . parse

parse :: String -> (Int, Int)
parse s = (players, finalP)
  where
    (players', rest) = span isDigit s
    players = read players'
    finalP = read $ takeWhile isDigit $ dropWhile (not . isDigit) rest

solve :: (Int, Int) -> Int
solve (players, finalP) = go 1 (M.fromList [(p, 0) | p <- [1..players]]) [0]
  where
    go next scores marbles
      | next == finalP + 1 = maximum $ map snd $ M.toList scores
      | next `mod` 23 == 0 =
        let (front, back) = splitAt (length marbles - 7) marbles
            scores' = M.adjust (+ (next + head back)) (next `mod` players) scores in
        go (next + 1) scores' (tail back ++ front)
      | otherwise = go (next + 1) scores (next : drop 2 marbles ++ take 2 marbles)
