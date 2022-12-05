open! Imports

module M = struct
  type t = int list

  let parse_entry entry =
    let entry =
      String.map
        ~f:(fun ch -> if Char.(ch = 'R' || ch = 'B') then '1' else '0')
        entry
    in
    Int.of_string ("0b" ^ entry)

  let parse inputs =
    let entries = String.split ~on:'\n' inputs in
    let entries = List.map ~f:parse_entry entries in
    entries

  let%expect_test _ =
    print_endline_int @@ parse_entry "FBFBBFFRLR" ;
    print_endline_int @@ parse_entry "BFFFBBFRRR" ;
    print_endline_int @@ parse_entry "FFFBBBFRRR" ;
    print_endline_int @@ parse_entry "BBFFBBFRLL" ;
    [%expect {|
      357
      567
      119
      820|}]

  let part1 entries =
    let ans = List.max_elt ~compare entries in
    print_endline_int @@ Option.value_exn ans

  let part2 entries =
    let sorted = List.sort ~compare entries in
    match sorted with
    | [] -> assert false
    | hd :: tl ->
        List.fold_until ~init:hd
          ~f:(fun last curr ->
            if last + 1 = curr then Continue curr
            else Stop (print_endline_int (last + 1)) )
          ~finish:(fun _ -> assert false)
          tl
end

include Day.Make (M)
