import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Data.List (nub)

main :: IO ()
main = interact $ show . solve . parse

parse :: String -> (S.Set Int, M.Map String Char)
parse s = (hashes, patterns)
  where
    init' : _ : patterns' = lines s
    init = drop (length "initial state: ") init'
    hashes = S.fromList $ map fst $ filter (\(_, x) -> x == '#') $ zip [0..] init
    patterns = M.fromList $ map (\s -> (take 5 s, s !! 9)) patterns'

solve :: (S.Set Int, M.Map String Char) -> Int
solve (s, patterns) = sum $ go 500000 s
  where
    go 0 acc = acc
    go n acc = go (n-1) $ S.fromList $ map fst $ filter (\(_, x) -> x == '#') [(i, next i) | i <- nub $ concatMap (\i -> [i-2..i+2]) $ S.elems acc]
      where
        next i = fromMaybe '.' $ patterns M.!? get5 i
        get5 i = map g [i-2..i+2]
          where
            g idx
              | idx `S.member` acc = '#'
              | otherwise          = '.'
