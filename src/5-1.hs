import Data.Char (toUpper, toLower)

remove :: String -> String
remove = reverse . snd . foldl go (' ', [])
  where
    go (prev, acc) curr
      | reactive prev curr = (if null $ tail acc then ' ' else head $ tail acc, tail acc)
      | otherwise = (curr, curr : acc)
    reactive x y
      = toUpper x == y && toLower y == x
        || toUpper y == x && toLower x == y

main :: IO ()
main = interact $ show . length . remove
