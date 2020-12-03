open! Base
open! Stdio

let part1 lines =
  let width = List.hd_exn lines |> String.length in
  let next x = (x + 3) % width in
  let f (acc, x) line =
    let acc = if Char.equal line.[x] '#' then acc + 1 else acc in
    (acc, next x)
  in
  let ans, _ = List.fold ~init:(0, 0) ~f lines in
  ans

let day3 inputs =
  let lines = String.split ~on:'\n' inputs in
  let answer1 = part1 lines in
  print_endline (Int.to_string answer1)

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

let%expect_test _ = day3 example ; [%expect {|7
  |}]
