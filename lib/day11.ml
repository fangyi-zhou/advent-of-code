open Imports

module M = struct
  type tile = Floor | Empty | Occupied

  let tile_of_char = function
    | 'L' -> Empty
    | '#' -> Occupied
    | '.' -> Floor
    | _ -> assert false

  type t = tile Array.t Array.t * int * int

  let parse inputs =
    let lines = String.split ~on:'\n' inputs in
    let dimx = List.length lines in
    let dimy = String.length (List.hd_exn lines) in
    let matrix = Array.make_matrix ~dimx ~dimy Empty in
    List.iteri
      ~f:(fun x line ->
        String.iteri ~f:(fun y ch -> matrix.(x).(y) <- tile_of_char ch) line
        )
      lines ;
    (matrix, dimx, dimy)

  let neighbours =
    List.cartesian_product [-1; 0; 1] [-1; 0; 1]
    |> List.filter ~f:(fun (x, y) -> x <> 0 || y <> 0)

  (* let print tiles =
   *   Array.iter ~f:(fun row ->
   *     Array.iter ~f:(fun tile ->
   *       let ch = match tile with Empty -> 'L' | Occupied -> '#' | Floor -> '.'
   *       in
   *       Stdio.printf "%c" ch
   *     ) row ;
   *     Stdio.print_endline ""
   *   ) tiles
   *)
  let count_occupied =
    Array.sum
      (module Int)
      ~f:(Array.sum (module Int) ~f:(function Occupied -> 1 | _ -> 0))

  let fixpoint original ~dimx ~dimy ~new_tile =
    let rec compute old =
      let curr = Array.make_matrix ~dimx ~dimy Empty in
      let updated = ref false in
      Array.iteri
        ~f:(fun x row ->
          Array.iteri
            ~f:(fun y old_tile ->
              let curr_tile = new_tile old x y old_tile in
              curr.(x).(y) <- curr_tile ;
              if Poly.(old_tile <> curr_tile) then updated := true )
            row )
        old ;
      (* print old ;
       * Stdio.print_endline "-->" ;
       * print curr ;
       * Stdio.print_endline "---" ; *)
      if !updated then compute curr else curr
    in
    compute original

  let part1 (original, dimx, dimy) =
    let new_tile old x y = function
      | Floor -> Floor
      | curr ->
          let occupied_counts =
            List.count
              ~f:(fun (dx, dy) ->
                try Poly.(old.(x + dx).(y + dy) = Occupied) with _ -> false
                )
              neighbours
          in
          if occupied_counts = 0 && Poly.(curr = Empty) then Occupied
          else if occupied_counts >= 4 && Poly.(curr = Occupied) then Empty
          else curr
    in
    let final = fixpoint ~dimx ~dimy ~new_tile original in
    let ans = count_occupied final in
    print_endline_int ans

  let part2 (original, dimx, dimy) =
    let new_tile old x y = function
      | Floor -> Floor
      | curr ->
          let occupied_counts =
            List.count
              ~f:(fun (dx, dy) ->
                try
                  let rec aux idx =
                    match old.(x + (idx * dx)).(y + (idx * dy)) with
                    | Floor -> aux (idx + 1)
                    | Empty -> false
                    | Occupied -> true
                  in
                  aux 1
                with _ -> false )
              neighbours
          in
          if occupied_counts = 0 && Poly.(curr = Empty) then Occupied
          else if occupied_counts >= 5 && Poly.(curr = Occupied) then Empty
          else curr
    in
    let final = fixpoint ~dimx ~dimy ~new_tile original in
    let ans = count_occupied final in
    print_endline_int ans
end

include Day.Make (M)

let example =
  "L.LL.LL.LL\n\
   LLLLLLL.LL\n\
   L.L.L..L..\n\
   LLLL.LL.LL\n\
   L.LL.LL.LL\n\
   L.LLLLL.LL\n\
   ..L.L.....\n\
   LLLLLLLLLL\n\
   L.LLLLLL.L\n\
   L.LLLLL.LL"

let%expect_test _ = run example ; [%expect {|
  37
  26|}]
