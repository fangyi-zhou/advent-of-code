main :: IO ()
main = interact $ show . solve . map read . words

solve :: [Int] -> Int
solve = fst . go 0
  where
    go acc [] = (acc, [])
    go acc (0 : metadata : rest) =
      let (meta, rest') = splitAt metadata rest in
      (acc + sum meta, rest')
    go acc (n : metadata : rest) =
      let (acc', rest') = go acc rest in
      go acc' ((n-1) : metadata : rest')

