import qualified Data.Map as M
import Prelude hiding (LT)
import Data.List (sort, group, maximumBy)
import Data.Function (on)
import Data.Maybe (fromMaybe)

type Track = M.Map (Int, Int) Tile
data Tile = HT | VT | IT | BS | FS deriving Show
type Car = ((Int, Int), (Turn, Direction))
data Turn = LT | ST | RT deriving Show
data Direction = L | R | U | D deriving Show

nextTurn :: Turn -> Turn
nextTurn LT = ST
nextTurn ST = RT
nextTurn RT = LT

main :: IO ()
main = interact $ show . solve . parse . lines

findCrash :: [Car] -> Maybe (Int, Int)
findCrash cars = case maximumBy (compare `on` fst) $ map (\x -> (length x, head x)) $ group $ sort $ map fst cars of
                   (1, _) -> Nothing
                   (_, pos) -> Just pos

nextPos :: (Int, Int) -> Direction -> (Int, Int)
nextPos (x, y) L = (x-1, y)
nextPos (x, y) R = (x+1, y)
nextPos (x, y) U = (x, y-1)
nextPos (x, y) D = (x, y+1)

move :: Track -> [Car] -> [Car]
move track = map moveSingle
  where
    moveSingle ((x, y), (turn, dir))
      = case track M.! nextPos (x, y) dir of
          HT -> (nextPos (x, y) dir, (turn, dir))
          VT -> (nextPos (x, y) dir, (turn, dir))
          IT -> (nextPos (x, y) dir, (nextTurn turn, mkTurn turn dir))
          BS -> (nextPos (x, y) dir, (turn, nextDir BS dir))
          FS -> (nextPos (x, y) dir, (turn, nextDir FS dir))
    nextDir BS L = U
    nextDir BS U = L
    nextDir BS R = D
    nextDir BS D = R
    nextDir FS L = D
    nextDir FS U = R
    nextDir FS R = U
    nextDir FS D = L
    mkTurn ST d = d
    mkTurn LT L = D
    mkTurn LT R = U
    mkTurn LT D = R
    mkTurn LT U = L
    mkTurn RT L = U
    mkTurn RT R = D
    mkTurn RT D = L
    mkTurn RT U = R

solve :: (Track, [Car]) -> (Int, Int)
solve (track, cars)
  = fromMaybe (solve (track, move track cars)) $ findCrash cars

parse :: [String] -> (Track, [Car])
parse lines = foldl parse' (M.empty, []) $ zip [0..] lines
  where
    parse' acc (y, line) = foldl update acc $ zip [0..] line
      where
        update (track, cars) (x, ch)
          | ch == '|' = (M.insert (x, y) VT track, cars)
          | ch == '-' = (M.insert (x, y) HT track, cars)
          | ch == '+' = (M.insert (x, y) IT track, cars)
          | ch == '/' = (M.insert (x, y) FS track, cars)
          | ch == '\\' = (M.insert (x, y) BS track, cars)
          | ch == '^' = (M.insert (x, y) VT track, ((x, y), (LT, U)) : cars)
          | ch == 'v' = (M.insert (x, y) VT track, ((x, y), (LT, D)) : cars)
          | ch == '<' = (M.insert (x, y) HT track, ((x, y), (LT, L)) : cars)
          | ch == '>' = (M.insert (x, y) HT track, ((x, y), (LT, R)) : cars)
          | ch == ' ' = (track, cars)
