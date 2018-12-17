import qualified Data.Map as M

immediatelyAfter = 640441

main :: IO ()
main = print $ solve immediatelyAfter

solve :: Int -> String
solve n = concatMap show [results M.! idx | idx <- [n..n+9]]
  where
    results = fst $ mk (n+10)
    mk 0 = (M.fromList [(0, 3), (1, 7)], (0, 1, 2))
    mk x = case mk (x-1) of
             (acc, (idx1, idx2, max)) ->
               if acc M.! idx1 + acc M.! idx2 >= 10
                  then (M.insert (max+1) ((acc M.! idx1 + acc M.! idx2) `mod` 10) $ M.insert max 1 acc, ((1 + idx1 + acc M.! idx1) `mod` (max + 2), (1 + idx2 + acc M.! idx2) `mod` (max + 2), max + 2))
                  else (M.insert max (acc M.! idx1 + acc M.! idx2) acc, ((1 + idx1 + acc M.! idx1) `mod` (max + 1), (1 + idx2 + acc M.! idx2) `mod` (max + 1), max + 1))
