import qualified Data.IntMap as M
import Control.Applicative ((<$>))
import Debug.Trace

sequenceToMatch :: [Int]
sequenceToMatch = [6,4,0,4,4,1]

main :: IO ()
main = print $ solve sequenceToMatch

solve :: [Int] -> Int
solve seq = go (M.fromList [(0, 3), (1, 7)], (0, 1, 2)) [3, 7]
  where
    go (acc, (idx1, idx2, max)) lasts
      = case hasAnswers lasts of
        Just offset -> max - offset - length seq
        Nothing ->
          let v1 = acc M.! idx1
              v2 = acc M.! idx2
              v1' = (1 + idx1 + v1) `mod` max'
              v2' = (1 + idx2 + v2) `mod` max'
              max' = if v1 + v2 >= 10 then max + 2 else max + 1
          in
          if v1 + v2 >= 10
            then go (M.insert (max+1) ((v1 + v2) `mod` 10) $ M.insert max 1 acc, (v1', v2', max')) $ truncate $ (v1 + v2) `mod` 10 : 1 : lasts
            else go (M.insert max (v1 + v2) acc, (v1', v2', max')) $ truncate $ (v1 + v2) : lasts
        where
          truncate = take (length seq + 1)
          seq' = reverse seq
          hasAnswers xs
            | length xs == length seq = if xs == seq' then Just 0 else Nothing
            | length xs < length seq = Nothing
            | otherwise = if take (length seq) xs == seq' then Just 0 else (+1) <$> hasAnswers (tail xs)
