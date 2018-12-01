import Data.List  (replicate)
import Data.Maybe (fromJust)
import qualified Data.Set as S

main :: IO ()
main = interact $ show . solve . (map (read . dropWhile (== '+'))) . lines

solve :: [Int] -> Int
solve xs = fromJust . fst $ foldl f (Nothing, S.singleton 0) $ scanl1 (+) $ concat $ replicate (length xs) xs
  where
    f :: (Maybe Int, S.Set Int) -> Int -> (Maybe Int, S.Set Int)
    f acc@(Just _, _) _ = acc
    f (Nothing, acc) x
      | S.member x acc = (Just x, acc)
      | otherwise  = (Nothing, S.insert x acc)

