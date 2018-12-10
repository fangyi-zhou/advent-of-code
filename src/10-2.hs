import Data.List (minimumBy, sort, intercalate)
import Data.Function (on)

main :: IO ()
main = interact $ show . solve . map parse . lines

parse :: String -> (Int, Int, Int, Int)
parse s = (x, y, vx, vy)
  where
    x  = read $ take 6 $ drop 10 s
    y  = read $ take 6 $ drop 18 s
    vx = read $ take 2 $ drop 36 s
    vy = read $ take 2 $ drop 40 s

solve :: [(Int, Int, Int, Int)] -> Int
solve stars = snd $ minimumBy d [([(x+vx*t, y+vy*t) | (x, y, vx, vy) <- stars], t) | t <- [0.. 30000]]
  where
    d = compare `on` f . fst
    f xs = range fst xs + range snd xs
    range f xs = maximum (map f xs) - minimum (map f xs)
