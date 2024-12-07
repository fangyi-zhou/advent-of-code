open! Imports

module M = struct
  type t = char Array.t Array.t

  let parse inputs =
    let lines = String.split_lines inputs in
    Array.of_list (List.map ~f:String.to_array lines)

  let part1 search =
    let count = ref 0 in
    let max_x = Array.length search in
    let max_y = Array.length search.(0) in
    let directions =
      [(0, 1); (1, 1); (1, 0); (1, -1); (0, -1); (-1, -1); (-1, 0); (-1, 1)]
    in
    let expected = Array.of_list ['X'; 'M'; 'A'; 'S'] in
    let find i j =
      List.iter
        ~f:(fun (dx, dy) ->
          let rec aux n =
            if n = 4 then count := !count + 1
            else
              let x = i + (n * dx) in
              let y = j + (n * dy) in
              if
                x >= 0 && x < max_x && y >= 0 && y < max_y
                && Char.equal search.(x).(y) expected.(n)
              then aux (n + 1)
          in
          aux 0 )
        directions
    in
    for i = 0 to max_x - 1 do
      for j = 0 to max_y - 1 do
        find i j
      done
    done ;
    print_endline_int !count

  let part2 search =
    let count = ref 0 in
    let max_x = Array.length search in
    let max_y = Array.length search.(0) in
    let expects =
      [ [(0, 0, 'M'); (2, 0, 'M'); (1, 1, 'A'); (0, 2, 'S'); (2, 2, 'S')]
      ; [(0, 0, 'S'); (2, 0, 'S'); (1, 1, 'A'); (0, 2, 'M'); (2, 2, 'M')]
      ; [(0, 0, 'M'); (2, 0, 'S'); (1, 1, 'A'); (0, 2, 'M'); (2, 2, 'S')]
      ; [(0, 0, 'S'); (2, 0, 'M'); (1, 1, 'A'); (0, 2, 'S'); (2, 2, 'M')] ]
    in
    let find i j =
      if
        List.exists
          ~f:(fun expected ->
            List.for_all
              ~f:(fun (dx, dy, expected_char) ->
                let x = i + dx in
                let y = j + dy in
                x >= 0 && x < max_x && y >= 0 && y < max_y
                && Char.equal search.(x).(y) expected_char )
              expected )
          expects
      then count := !count + 1
    in
    for i = 0 to max_x - 1 do
      for j = 0 to max_y - 1 do
        find i j
      done
    done ;
    print_endline_int !count
end

include M
include Day.Make (M)

let example =
  "MMMSXXMASM\n\
   MSAMXMSMSA\n\
   AMXSXMAAMM\n\
   MSAMASMSMX\n\
   XMASAMXAMM\n\
   XXAMMXXAMA\n\
   SMSMSASXSS\n\
   SAXAMASAAA\n\
   MAMMMXMMMM\n\
   MXMXAXMASX"

let%expect_test _ =
  run example ;
  [%expect
    {|
                                    18
                                    9
                                    |}]
