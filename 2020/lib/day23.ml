open Imports

module Ring = struct
  type t = int list * int * int list

  let shift_right = function
    | hd, here, next :: tl -> (here :: hd, next, tl)
    | hd, here, [] -> (
        let hd_rev = List.rev hd in
        match hd_rev with
        | [] -> ([], here, []) (* Singleton, for completeness *)
        | next :: tl -> ([here], next, tl) )

  let shift_left = function
    | prev :: hd, here, tl -> (hd, prev, here :: tl)
    | [], here, tl -> (
        let tl_rev = List.rev tl in
        match tl_rev with
        | [] -> ([], here, [])
        | prev :: hd -> (hd, prev, [here]) )

  let of_list = function [] -> assert false | hd :: tl -> ([], hd, tl)

  let current = function _, curr, _ -> curr

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

  let focus elem = function
    | (_, curr, _) as ring when compare elem curr = 0 -> ring
    | ring ->
        let rec aux left right =
          if compare (current left) elem = 0 then left
          else if compare (current right) elem = 0 then right
          else aux (shift_left left) (shift_right right)
        in
        aux (shift_left ring) (shift_right ring)

  (* n must be greater than ring size *)
  let rec remove_right n = function
    | hd, curr, tl when List.length tl < n ->
        remove_right n ([], curr, tl @ List.rev hd)
    | hd, curr, tl ->
        let removed, tl = List.split_n tl n in
        (removed, (hd, curr, tl))

  let insert_right elems = function hd, curr, tl -> (hd, curr, elems @ tl)

  let to_list = function hd, curr, tl -> [curr] @ tl @ List.rev hd
end

module Ring_Map = struct
  (* Each map entry corresponds to next number and prev number; integer
     represents the focus *)

  type t = (int * int) Map.M(Int).t * int

  let focus elem (entries, _) = (entries, elem)

  let remove_right n (entries, curr) =
    let rec aux acc iter entries =
      if iter = n then (List.rev acc, (entries, curr))
      else
        let _, to_remove = Map.find_exn entries curr in
        let _, to_fix = Map.find_exn entries to_remove in
        let entries = Map.remove entries to_remove in
        let entries =
          Map.update entries curr ~f:(function
            | Some (prev, _) -> (prev, to_fix)
            | None -> assert false )
        in
        let entries =
          Map.update entries curr ~f:(function
            | Some (_, next) -> (curr, next)
            | None -> assert false )
        in
        aux (to_remove :: acc) (iter + 1) entries
    in
    aux [] 0 entries

  let of_list xs =
    match xs with
    | [] -> assert false
    | hd :: tl ->
        let interleaved = List.zip_exn xs (tl @ [hd]) in
        ( List.fold
            ~init:(Map.empty (module Int))
            ~f:(fun acc (prev, curr) ->
              let acc =
                Map.update acc curr ~f:(function
                  | None -> (prev, 0)
                  | Some (_, next) -> (prev, next) )
              in
              let acc =
                Map.update acc prev ~f:(function
                  | None -> (0, curr)
                  | Some (prevprev, _) -> (prevprev, curr) )
              in
              acc )
            interleaved
        , hd )

  let current (_, curr) = curr

  let shift_right (entries, curr) =
    let _, next = Map.find_exn entries curr in
    (entries, next)

  let insert_right elems (entries, curr) =
    let rev_elems = List.rev elems in
    ( List.fold ~init:entries
        ~f:(fun entries new_elem ->
          let _, to_fix = Map.find_exn entries curr in
          let entries =
            Map.add_exn entries ~key:new_elem ~data:(curr, to_fix)
          in
          let entries =
            Map.update entries curr ~f:(function
              | Some (prev, _) -> (prev, new_elem)
              | None -> assert false )
          in
          let entries =
            Map.update entries to_fix ~f:(function
              | Some (_, next) -> (new_elem, next)
              | None -> assert false )
          in
          entries )
        rev_elems
    , curr )

  let to_list (entries, focus) =
    let rec aux acc curr =
      let _, next = Map.find_exn entries curr in
      if next = focus then List.rev acc else aux (next :: acc) next
    in
    aux [focus] focus
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

  let count = ref 0

  let incr_count () = count := !count + 1

  let reset_count () = count := 0

  let move ~limit ring =
    incr_count () ;
    if !count % 100000 = 0 then print_endline_int !count ;
    let hd = Ring_Map.current ring in
    match Ring_Map.remove_right 3 ring with
    | [e1; e2; e3], ring ->
        let dest = find_dest (hd - 1) e1 e2 e3 ~limit in
        let ring = Ring_Map.focus dest ring in
        let ring = Ring_Map.insert_right [e1; e2; e3] ring in
        let ring = Ring_Map.focus hd ring in
        let ring = Ring_Map.shift_right ring in
        ring
    | _ -> assert false

  let move_with_print ~limit input =
    let output = move ~limit input in
    print_endline (string_of_list @@ Ring_Map.to_list output) ;
    output

  let part1 init =
    reset_count () ;
    let init = Ring_Map.of_list init in
    let final = Fn.apply_n_times (move ~limit:9) ~n:100 init in
    let final = Ring_Map.focus 1 final in
    let final = Ring_Map.to_list final in
    let ans = string_of_list (List.tl_exn final) in
    print_endline ans

  let part2 init =
    reset_count () ;
    let init = init @ List.range ~stop:`inclusive 10 1000000 in
    let init = Ring_Map.of_list init in
    let final = Fn.apply_n_times (move ~limit:1000000) ~n:10000000 init in
    let final = Ring_Map.focus 1 final in
    let two_to_the_right, _ = Ring_Map.remove_right 2 final in
    let ans = List.product two_to_the_right in
    print_endline_int ans
end

include Day.Make (M)

let example = "389125467"

let%expect_test _ =
  run ~only_part1:true example ;
  [%expect {| 67384529 |}]
