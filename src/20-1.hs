main :: IO ()
main = interact $ show . solve

--solve :: String -> Int
solve = maximum . map length . fst . solve' [] [""]
--  where
solve' :: [String] -> [String] -> String -> ([String], String)
solve' acc curr [] = (curr ++ acc, [])
solve' acc curr (')':rest) = (curr ++ acc, rest)
solve' acc curr ('$':rest) = (curr ++ acc, rest)
solve' acc curr ('|':rest) = solve' (curr ++ acc) [""] rest
solve' acc curr ('(':rest) = let (n, rest') = solve' [] [""] rest in solve' acc [foldr prepend curr_ n_ | n_ <- n, curr_ <- curr] rest'
solve' acc curr ('^':rest) = let (n, rest') = solve' [] [""] rest in solve' acc [foldr prepend curr_ n_ | n_ <- n, curr_ <- curr] rest'
solve' acc curr ( x :rest) = solve' acc (map (prepend x) curr) rest

prepend x [] = [x]
prepend 'S' ('N':ys) = ys
prepend 'N' ('S':ys) = ys
prepend 'W' ('E':ys) = ys
prepend 'E' ('W':ys) = ys
prepend x ys = x : ys
