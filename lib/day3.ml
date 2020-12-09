open! Imports

module M = struct
  type t = string list

  let parse inputs = String.split ~on:'\n' inputs

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

  let part1 lines =
    let ans = traverse lines (3, 1) in
    print_endline_int ans

  let part2 lines =
    let patterns = [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)] in
    let trees = List.map ~f:(traverse lines) patterns in
    let ans = List.fold ~init:1 ~f:(fun x y -> x * y) trees in
    print_endline_int ans
end

include Day.Make (M)

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

let%expect_test _ = run example ; [%expect {|
  7
  336|}]
