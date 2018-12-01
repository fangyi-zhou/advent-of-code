main :: IO ()
main = interact $ show . sum . (map (read . dropWhile (== '+'))) . lines
-- dropWhile is needed because `read` doesn't parse + before numbers
