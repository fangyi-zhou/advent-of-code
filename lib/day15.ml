open Imports

module M = struct
  type t = int list

  let parse inputs =
    let nums = String.split ~on:',' inputs in
    List.map ~f:Int.of_string nums

  (* After frustration of finding ways to compute the sequence efficiently, I
   * gave up and checked how other people did it. It turns out that the key is
   * to use a hashmap with a huge size to prevent allocation, so sorry my old
   * friend Map. *)
  let run_game init until =
    let memory = Hashtbl.create ~size:until (module Int) in
    let rec aux init turn prev =
      if turn = until then prev
      else
        let update_mem () = Hashtbl.update memory prev ~f:(fun _ -> turn) in
        match init with
        | hd :: tl ->
            update_mem () ;
            aux tl (turn + 1) hd
        | [] ->
            let next =
              match Hashtbl.find memory prev with
              | None -> 0
              | Some v -> turn - v
            in
            update_mem () ;
            aux init (turn + 1) next
    in
    let ans = aux init 0 (-1) in
    ans

  let part1 init =
    let ans = run_game init 2020 in
    print_endline_int ans

  let part2 init =
    let ans = run_game init 30000000 in
    print_endline_int ans
end

include M
include Day.Make (M)

let%expect_test _ = run "0,3,6" ; [%expect {|
  436
  175594 |}]

let%expect_test _ = run "1,3,2" ; [%expect {|
  1
  2578 |}]

let%expect_test _ = run "2,1,3" ; [%expect {|
  10
  3544142 |}]

let%expect_test _ = run "1,2,3" ; [%expect {|
  27
  261214 |}]

let%expect_test _ = run "2,3,1" ; [%expect {|
  78
  6895259 |}]

let%expect_test _ = run "3,2,1" ; [%expect {|
  438
  18 |}]

let%expect_test _ = run "3,1,2" ; [%expect {|
  1836
  362 |}]
