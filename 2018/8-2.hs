import Data.Maybe (fromMaybe)

main :: IO ()
main = interact $ show . solve . map read . words

solve :: [Int] -> Int
solve = fst . go

go (0 : metadata : rest) =
  let (meta, rest') = splitAt metadata rest in
  (sum meta, rest')
go (n : metadata : rest) =
  let (meta, rest'') = splitAt metadata rest'
      (vals, rest') = foldl go' ([], rest) [1..n]
      go' (a, r) id = let (v, r') = go r in ((id, v) : a, r')
      val idx = fromMaybe 0 $ lookup idx vals in
  (sum $ map val meta, rest'')

