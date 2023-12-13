open Imports

module M = struct
  type t = int * (int * int) list

  let parse inputs =
    let lines = String.split ~on:'\n' inputs in
    match lines with
    | [timestamp; intervals] ->
        let timestamp = Int.of_string timestamp in
        let intervals = String.split ~on:',' intervals in
        let intervals =
          List.filter_mapi
            ~f:(fun idx -> function
              | "x" -> None | interval -> Some (Int.of_string interval, idx) )
            intervals
        in
        (timestamp, intervals)
    | _ -> assert false

  let part1 (timestamp, intervals) =
    let intervals = List.map ~f:fst intervals in
    (* Observation from input, all inputs are prime *)
    let lcm = List.product intervals in
    let timestamp = timestamp % lcm in
    let ans =
      Sequence.fold_until ~init:()
        ~f:(fun () curr_time ->
          match
            List.find intervals ~f:(fun interval -> curr_time % interval = 0)
          with
          | Some interval -> Stop (interval * (curr_time - timestamp))
          | None -> Continue () )
        ~finish:(fun _ -> assert false)
        (Sequence.range timestamp lcm)
    in
    print_endline_int ans

  let part2 (_, intervals) =
    let ans =
      let f (multiples, curr) (prime, modulo) =
        let rec aux curr =
          if (curr + modulo) % prime = 0 then curr else aux (curr + multiples)
        in
        (multiples * prime, aux curr)
      in
      let _, ans = List.fold1 ~f intervals in
      ans
    in
    print_endline_int ans
end

include Day.Make (M)

let example = "939\n7,13,x,x,59,x,31,19"

let%expect_test _ = run example ; [%expect {|
  295
  1068781 |}]
