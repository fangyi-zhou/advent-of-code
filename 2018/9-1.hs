import Data.Char (isDigit)
import qualified Data.Map as M

data Cycle = C [Int] Int [Int]
instance Show Cycle where
  show (C l x r) = show $ reverse l ++ [x] ++ r

left :: Cycle -> Int -> Cycle
left c 0 = c
left (C [] x []) _ = C [] x []
left (C [] x r) n = left (C (reverse r) x []) n
left (C (l:xs) x r) n = left (C xs l (x:r)) (n-1)

right :: Cycle -> Int -> Cycle
right c 0 = c
right (C [] x []) _ = C [] x []
right (C l x []) n = right (C [] x (reverse l)) n
right (C l x (r:xs)) n = right (C (x:l) r xs) (n-1)

curr :: Cycle -> Int
curr (C _ x _) = x

removeR :: Cycle -> Cycle
removeR (C l x []) = removeR (C [] x (reverse l))
removeR (C l x (r:xs)) = C l r xs

addL :: Cycle -> Int -> Cycle
addL (C l x r) v = C (x:l) v r

main :: IO ()
main = interact $ show . solve . parse

parse :: String -> (Int, Int)
parse s = (players, finalP)
  where
    (players', rest) = span isDigit s
    players = read players'
    finalP = read $ takeWhile isDigit $ dropWhile (not . isDigit) rest

solve :: (Int, Int) -> Int
solve (players, finalP) = go 1 (M.fromList [(p, 0) | p <- [1..players]]) $ C [] 0 []
  where
    go next scores marbles
      | next == finalP + 1 = maximum $ map snd $ M.toList scores
      | next `mod` 23 == 0 =
        let marbles' = left marbles 7
            scores' = M.adjust (+ (next + curr marbles')) (next `mod` players) scores in
        go (next + 1) scores' (removeR marbles')
      | otherwise =
        let marbles' = right marbles 1 in
        go (next + 1) scores (addL marbles' next)
