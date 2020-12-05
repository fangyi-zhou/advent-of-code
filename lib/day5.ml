open! Base
open! Stdio

module M = struct
  type t = string list

  let parse = String.split ~on:'\n'

  let parse_entry entry =
    let entry =
      String.map
        ~f:(fun ch -> if Char.(ch = 'R' || ch = 'B') then '1' else '0')
        entry
    in
    Int.of_string ("0b" ^ entry)

  let%expect_test _ =
    print_endline (Int.to_string @@ parse_entry "FBFBBFFRLR") ;
    print_endline (Int.to_string @@ parse_entry "BFFFBBFRRR") ;
    print_endline (Int.to_string @@ parse_entry "FFFBBBFRRR") ;
    print_endline (Int.to_string @@ parse_entry "BBFFBBFRLL") ;
    [%expect {|
      357
      567
      119
      820|}]

  let part1 entries =
    let entries = List.map ~f:parse_entry entries in
    let ans = List.max_elt ~compare entries in
    print_endline (Int.to_string @@ Option.value_exn ans)

  let part2 _ = ()
end

include Day.Make (M)
