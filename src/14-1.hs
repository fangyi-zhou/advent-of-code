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
               let v1 = acc M.! idx1
                   v2 = acc M.! idx2
                   v1' = (1 + idx1 + v1) `mod` max'
                   v2' = (1 + idx2 + v2) `mod` max'
                   max' = if v1 + v2 >= 10 then max + 2 else max + 1
               in
               if v1 + v2 >= 10
                  then (M.insert (max+1) ((v1 + v2) `mod` 10) $ M.insert max 1 acc, (v1', v2', max'))
                  else (M.insert max (v1 + v2) acc, (v1', v2', max'))
