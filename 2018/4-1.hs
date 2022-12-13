import Data.Char (isDigit)
import Data.Function (on)
import Data.List (group, sort, minimumBy, maximumBy)
import qualified Data.IntMap as M

main :: IO ()
main = interact $ show . solve . parse . sort . lines

solve :: M.IntMap [Int] -> Int
solve = g . maximumBy (compare `on` snd). M.toList . fmap aggregate
  where
    g (id, (_, min)) = id * min
    findMinute = snd . minimumBy (flip compare). map (\g -> (length g, head g)) . group . sort
    aggregate :: [Int] -> (Int, Int)
    aggregate sleeps = (length sleeps, findMinute sleeps)

parse :: [String] -> M.IntMap [Int]
parse = fst . foldl go init
  where
    init = (M.empty, (-1, Nothing))
    readGuard s = read $ takeWhile isDigit $ drop (length "[1518-11-01 00:00] Guard #") s
    readMinute s = read $ takeWhile isDigit $ drop (length "[1518-11-01 00:") s
    add g acc
      | g `M.member` acc = acc
      | otherwise = M.insert g [] acc
    go (acc, (guard, start)) s
      | '#' `elem` s =
        let newG = readGuard s in (add newG acc, (newG, Nothing))
      | 'f' `elem` s = (acc, (guard, Just $ readMinute s))
      | 'w' `elem` s = (update acc guard start $ readMinute s, (guard, Nothing))
    update acc g (Just s) e
      = M.adjust ([s .. e-1] ++) g acc
