import qualified Data.Set as S
import Data.List (sortBy, (\\))

type Graph = (S.Set Char, S.Set (Char, Char))

main :: IO ()
main = interact $ toposort . parse . lines

parse :: [String] -> Graph
parse ss = (nodes, edges)
  where
    edges = S.fromList $ map (\s -> (s !! 5, s !! 36)) ss
    nodes = S.map fst edges `S.union` S.map snd edges

comp (n1, d1) (n2, d2)
  | d1 == d2 = compare n1 n2
  | otherwise = compare d1 d2

toposort :: Graph -> String
toposort g = reverse $ go (mkDegrees g) "" g
  where
    mkDegrees :: Graph -> [(Char, Int)]
    mkDegrees (nodes, edges) = sortBy comp [(node, indeg node edges) | node <- S.toList nodes]
      where
        indeg node = S.size . S.filter (\(from, to) -> to == node)
    go degs acc (nodes, edges)
      | null degs = acc
      | otherwise = go (sortBy comp $ removeDeg degs' zero) (zero : acc) (nodes, edges)
        where
          ((zero, _): degs') = degs
          removeDeg :: [(Char, Int)] -> Char -> [(Char, Int)]
          removeDeg degs nodeToRemove = map f degs
            where
              f (node, deg)
                | node `S.member` toRemove nodeToRemove = (node, deg-1)
                | otherwise = (node, deg)
          toRemove node = S.map snd $ S.filter (\(from, to) -> from == node) edges
