import Data.Array
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

main :: IO ()
main = interact $ show . solve . parse

parse :: String -> (String, M.Map String Char)
parse s = (init, patterns)
  where
    init' : _ : patterns' = lines s
    init = drop (length "initial state: ") init'
    patterns = M.fromList $ map (\s -> (take 5 s, s !! 9)) patterns'

solve :: (String, M.Map String Char) -> Int
solve (s, patterns) = sum $ map fst $ filter (\(_, x) -> x == '#') $ assocs $ go 20 $ array (0, length s-1) $ zip [0..] s
  where
    go 0 acc = acc
    go n acc = go (n-1) $ array bounds' [(i, next i) | i <- range bounds']
      where
        (start, end) = bounds acc
        bounds' = (start-2, end+2)
        next i = fromMaybe '.' $ patterns M.!? get5 i
        get5 i = map g [i-2..i+2]
          where
            g idx
              | inRange (start, end) idx = acc ! idx
              | otherwise                = '.'
