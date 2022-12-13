import qualified Data.Map.Strict as M
type Map = M.Map (Int, Int) Tile
data Tile = Open | Tree | Lumberyard deriving (Eq, Ord)

instance Show Tile where
  show Open = "."
  show Tree = "|"
  show Lumberyard = "#"

main :: IO ()
main = interact $ show . solve . parse

parse :: String -> Map
parse = foldl go M.empty . zip [0..] . lines
  where
    go acc (idxX, line) = foldl go' acc $ zip [0..] line
      where
        go' acc (idxY, ch)
          | ch == '.' = M.insert (idxX, idxY) Open acc
          | ch == '|' = M.insert (idxX, idxY) Tree acc
          | ch == '#' = M.insert (idxX, idxY) Lumberyard acc
          | otherwise = acc

n = 1000000000

solve :: Map -> Int
solve m = uncurry (*) $ count $ go (init + ((n - init) `mod` cycle)) m
  where
    (init, cycle) = findCycle m

count :: Map -> (Int, Int)
count = M.foldl count' (0, 0)
  where
    count' (t, l) Tree = (t+1, l)
    count' (t, l) Lumberyard = (t, l+1)
    count' (t, l) Open = (t, l)

findCycle :: Map -> (Int, Int)
findCycle m = find' 1 m $ M.singleton m 0
  where
    find' count curr acc
      | M.member (magic curr) acc = (acc M.! magic curr, count - acc M.! magic curr)
      | otherwise = find' (count+1) (magic curr) (M.insert (magic curr) count acc)

go :: Int -> Map -> Map
go 0 = id
go x = magic . go (x-1)

magic :: Map -> Map
magic curr = M.foldlWithKey f M.empty curr
  where
    f acc (x, y) Open
      | countSurround Tree (x, y) >= 3 = M.insert (x, y) Tree acc
      | otherwise                      = M.insert (x, y) Open acc
    f acc (x, y) Tree
      | countSurround Lumberyard (x, y) >= 3 = M.insert (x, y) Lumberyard acc
      | otherwise                            = M.insert (x, y) Tree acc
    f acc (x, y) Lumberyard
      | countSurround Tree (x, y) >= 1 && countSurround Lumberyard (x, y) >= 1 = M.insert (x, y) Lumberyard acc
      | otherwise                                                              = M.insert (x, y) Open acc
    getSurround (x, y) = [curr M.! (x_, y_) | x_ <- [x-1..x+1], y_ <- [y-1..y+1], not (x == x_ && y == y_), M.member (x_, y_) curr]
    countSurround t = length . filter (== t) . getSurround

printMap :: Map -> String
printMap m
  = unlines [mkLine x | x <- [minX..maxX]]
    where
      ((minX, minY), _) = M.findMin m
      ((maxX, maxY), _) = M.findMax m
      mkLine x
        = concat [show $ m M.! (x, y) | y <- [minY..maxY]]
