open! Base
open! Stdio

let traverse lines (dx, dy) =
  let width = List.hd_exn lines |> String.length in
  let next x = (x + dx) % width in
  let f (acc, x, y) line =
    let acc =
      if y % dy = 0 && Char.equal line.[x] '#' then acc + 1 else acc
    in
    let x = if y % dy = 0 then next x else x in
    (acc, x, y + 1)
  in
  let ans, _, _ = List.fold ~init:(0, 0, 0) ~f lines in
  ans

let part1 lines = traverse lines (3, 1)

let part2 lines =
  let patterns = [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)] in
  let trees = List.map ~f:(traverse lines) patterns in
  List.fold ~init:1 ~f:(fun x y -> x * y) trees

let day3 inputs =
  let lines = String.split ~on:'\n' inputs in
  let answer1 = part1 lines in
  let answer2 = part2 lines in
  print_endline (Int.to_string answer1) ;
  print_endline (Int.to_string answer2)

let example =
  "..##.......\n\
   #...#...#..\n\
   .#....#..#.\n\
   ..#.#...#.#\n\
   .#...##..#.\n\
   ..#.##.....\n\
   .#.#.#....#\n\
   .#........#\n\
   #.##...#...\n\
   #...##....#\n\
   .#..#...#.#"

let%expect_test _ = day3 example ; [%expect {|7 336|}]
