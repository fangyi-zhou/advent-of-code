open Imports

module M = struct
  type t = int list

  let parse str =
    List.map
      ~f:(fun ch -> Int.of_string (String.of_char ch))
      (String.to_list str)

  let shift = function hd :: tl -> tl @ [hd] | _ -> assert false

  let string_of_list lst =
    String.concat ~sep:"" (List.map ~f:Int.to_string lst)

  let rec find_dest curr e1 e2 e3 =
    match curr with
    | curr when curr = 0 -> find_dest 9 e1 e2 e3
    | curr when curr <> e1 && curr <> e2 && curr <> e3 -> curr
    | curr -> find_dest (curr - 1) e1 e2 e3

  let move = function
    | hd :: e1 :: e2 :: e3 :: tl ->
        let dest = find_dest (hd - 1) e1 e2 e3 in
        let idx, _ =
          Option.value_exn (List.findi ~f:(fun _ num -> num = dest) tl)
        in
        let prefix, suffix = List.split_n tl (idx + 1) in
        prefix @ [e1; e2; e3] @ suffix @ [hd]
    | _ -> assert false

  let move_with_print input =
    let output = move input in
    print_endline (string_of_list output) ;
    output

  let normalise ring =
    let prefix, suffix = List.split_while ~f:(fun i -> i <> 1) ring in
    suffix @ prefix

  let part1 init =
    let final = Fn.apply_n_times move ~n:100 init in
    let final = normalise final in
    let ans = string_of_list (List.tl_exn final) in
    print_endline ans

  let part2 _ = ()
end

include Day.Make (M)

let example = "389125467"

let%expect_test _ = run example ; [%expect {| 67384529 |}]
