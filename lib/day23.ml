open Imports

module Ring = struct
  type 'a t = Ring of 'a list * 'a * 'a list

  let shift_right = function
    | Ring (hd, here, next :: tl) -> Ring (here :: hd, next, tl)
    | Ring (hd, here, []) -> (
        let hd_rev = List.rev hd in
        match hd_rev with
        | [] -> Ring ([], here, []) (* Singleton, for completeness *)
        | next :: tl -> Ring ([here], next, tl) )

  let shift_left = function
    | Ring (prev :: hd, here, tl) -> Ring (hd, prev, here :: tl)
    | Ring ([], here, tl) -> (
        let tl_rev = List.rev tl in
        match tl_rev with
        | [] -> Ring ([], here, [])
        | prev :: hd -> Ring (hd, prev, [here]) )

  let of_list = function [] -> assert false | hd :: tl -> Ring ([], hd, tl)

  let current = function Ring (_, curr, _) -> curr

  (* elem must be in the ring *)
  (*
   * let rec focus_right ~compare elem = function
   *   | Ring (_, curr, _) as ring when compare elem curr = 0 -> ring
   *   | ring -> focus_right ~compare elem (shift_right ring)
   *
   * let rec focus_left ~compare elem = function
   *   | Ring (_, curr, _) as ring when compare elem curr = 0 -> ring
   *   | ring -> focus_left ~compare elem (shift_left ring)
   *)

  let focus ~compare elem = function
    | Ring (_, curr, _) as ring when compare elem curr = 0 -> ring
    | ring ->
        let rec aux left right =
          if compare (current left) elem = 0 then left
          else if compare (current right) elem = 0 then right
          else aux (shift_left left) (shift_right right)
        in
        aux (shift_left ring) (shift_right ring)

  (* n must be greater than ring size *)
  let rec remove_right n = function
    | Ring (hd, curr, tl) when List.length tl < n ->
        remove_right n (Ring ([], curr, tl @ List.rev hd))
    | Ring (hd, curr, tl) ->
        let removed, tl = List.split_n tl n in
        (removed, Ring (hd, curr, tl))

  let insert_right elems = function
    | Ring (hd, curr, tl) -> Ring (hd, curr, elems @ tl)

  let to_list = function Ring (hd, curr, tl) -> [curr] @ tl @ List.rev hd
end

module M = struct
  type t = int list

  let parse str =
    List.map
      ~f:(fun ch -> Int.of_string (String.of_char ch))
      (String.to_list str)

  let shift = function hd :: tl -> tl @ [hd] | _ -> assert false

  let string_of_list lst =
    String.concat ~sep:"" (List.map ~f:Int.to_string lst)

  let rec find_dest curr e1 e2 e3 ~limit =
    match curr with
    | curr when curr = 0 -> find_dest limit e1 e2 e3 ~limit
    | curr when curr <> e1 && curr <> e2 && curr <> e3 -> curr
    | curr -> find_dest (curr - 1) e1 e2 e3 ~limit

  let move ~limit ring =
    let hd = Ring.current ring in
    match Ring.remove_right 3 ring with
    | [e1; e2; e3], ring ->
        let dest = find_dest (hd - 1) e1 e2 e3 ~limit in
        let ring = Ring.focus ~compare dest ring in
        let ring = Ring.insert_right [e1; e2; e3] ring in
        let ring = Ring.focus ~compare hd ring in
        let ring = Ring.shift_right ring in
        ring
    | _ -> assert false

  let move_with_print ~limit input =
    let output = move ~limit input in
    print_endline (string_of_list @@ Ring.to_list output) ;
    output

  let part1 init =
    let init = Ring.of_list init in
    let final = Fn.apply_n_times (move ~limit:9) ~n:100 init in
    let final = Ring.focus ~compare 1 final in
    let final = Ring.to_list final in
    let ans = string_of_list (List.tl_exn final) in
    print_endline ans

  let _part2 init =
    let init = init @ List.range ~stop:`inclusive 10 1000000 in
    let init = Ring.of_list init in
    let final = Fn.apply_n_times (move ~limit:1000000) ~n:10000000 init in
    let final = Ring.focus ~compare 1 final in
    let two_to_the_right, _ = Ring.remove_right 2 final in
    let ans = List.product two_to_the_right in
    print_endline_int ans

  let part2 _ = ()
end

include Day.Make (M)

let example = "389125467"

let%expect_test _ = run example ; [%expect {| 67384529 |}]
