open! Base
open! Stdio

module M = struct
  type t = int list

  let parse inputs =
    let lines = String.split ~on:'\n' inputs in
    List.map ~f:Int.of_string lines

  let capacity = 25

  let part1 nums =
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
    print_endline (Int.to_string ans)

  let part2 _ = ()
end

include Day.Make (M)
