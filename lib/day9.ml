open! Imports

module M = struct
  type t = int list

  let parse inputs =
    let lines = String.split ~on:'\n' inputs in
    List.map ~f:Int.of_string lines

  let capacity = 25

  let part1_with_capacity ~capacity nums =
    let queue = Queue.create ~capacity () in
    let ans =
      List.fold_until ~init:()
        ~f:(fun _ num ->
          if Queue.length queue <= capacity then (
            Queue.enqueue queue num ; Continue () )
          else
            match
              Day1.sum2 num (Set.of_list (module Int) (Queue.to_list queue))
            with
            | Some _ ->
                Queue.enqueue queue num ;
                ignore (Queue.dequeue_exn queue) ;
                Continue ()
            | None -> Stop num)
        ~finish:(fun _ -> assert false)
        nums
    in
    ans

  let part1 nums =
    let ans = part1_with_capacity ~capacity nums in
    print_endline_int ans

  let part2_with_capacity ~capacity nums =
    let subseq_sum = part1_with_capacity ~capacity nums in
    let find_ans queue =
      Option.value_exn (Queue.min_elt queue ~compare)
      + Option.value_exn (Queue.max_elt queue ~compare)
    in
    let queue = Queue.create () in
    List.fold_until ~init:0
      ~f:(fun acc elem ->
        let rec aux acc elem : _ Continue_or_stop.t =
          let acc' = acc + elem in
          if acc' = subseq_sum then (
            Queue.enqueue queue elem ;
            Stop (find_ans queue) )
          else if acc' < subseq_sum then (
            Queue.enqueue queue elem ; Continue acc' )
          else
            let first = Queue.dequeue_exn queue in
            aux (acc - first) elem
        in
        aux acc elem)
      ~finish:(fun _ -> assert false)
      nums

  let part2 nums =
    let ans = part2_with_capacity ~capacity nums in
    print_endline_int ans
end

include M
include Day.Make (M)

let example =
  "35\n\
   20\n\
   15\n\
   25\n\
   47\n\
   40\n\
   62\n\
   55\n\
   65\n\
   95\n\
   102\n\
   117\n\
   150\n\
   182\n\
   127\n\
   219\n\
   299\n\
   277\n\
   309\n\
   576"

let%expect_test _ =
  let nums = parse example in
  let ans1 = part1_with_capacity ~capacity:5 nums in
  print_endline_int ans1 ;
  let ans2 = part2_with_capacity ~capacity:5 nums in
  print_endline_int ans2 ;
  [%expect {|
    127
    62
              |}]
