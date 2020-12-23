open Imports

module M = struct
  type t = int list * int list

  let parse inputs =
    let lines = String.split ~on:'\n' inputs in
    let parts =
      List.group
        ~break:(fun a b -> String.is_empty a || String.is_empty b)
        lines
    in
    match parts with
    | [player1; _; player2] ->
        let player1 = List.map ~f:Int.of_string (List.tl_exn player1) in
        let player2 = List.map ~f:Int.of_string (List.tl_exn player2) in
        (player1, player2)
    | _ -> assert false

  let part1 (player1, player2) =
    let player1 = Queue.of_list player1 in
    let player2 = Queue.of_list player2 in
    let rec aux () =
      match (Queue.peek player1, Queue.peek player2) with
      | Some e1, Some e2 ->
          let _ = Queue.dequeue player1 in
          let _ = Queue.dequeue player2 in
          if e1 > e2 then (
            Queue.enqueue player1 e1 ; Queue.enqueue player1 e2 ; aux () )
          else (Queue.enqueue player2 e2 ; Queue.enqueue player2 e1 ; aux ())
      | None, None -> assert false
      | None, _ -> player2
      | _, None -> player1
    in
    let winner_queue = aux () in
    let ans =
      Queue.foldi ~init:0
        ~f:(fun idx acc elem ->
          acc + (elem * (Queue.length winner_queue - idx)))
        winner_queue
    in
    print_endline_int ans

  let string_of_queues q1 q2 =
    let buffer = Buffer.create 128 in
    let append i =
      Buffer.add_string buffer (Int.to_string i) ;
      Buffer.add_char buffer ' '
    in
    Queue.iter ~f:append q1 ;
    Buffer.add_char buffer ';' ;
    Queue.iter ~f:append q2 ;
    Buffer.contents buffer

  let part2 (player1, player2) =
    let player1 = Queue.of_list player1 in
    let player2 = Queue.of_list player2 in
    let cache = ref (Map.empty (module String)) in
    let rec aux player1 player2 =
      match (Queue.peek player1, Queue.peek player2) with
      | Some e1, Some e2 ->
          let _ = Queue.dequeue player1 in
          let _ = Queue.dequeue player2 in
          let recursive_game =
            Queue.length player1 >= e1 && Queue.length player2 >= e2
          in
          let player1_wins =
            if recursive_game then (
              let new_player1 =
                Array.sub ~pos:0 ~len:e1 (Queue.to_array player1)
                |> Queue.of_array
              in
              let new_player2 =
                Array.sub ~pos:0 ~len:e2 (Queue.to_array player2)
                |> Queue.of_array
              in
              let cache_key = string_of_queues new_player1 new_player2 in
              match Map.find !cache cache_key with
              | Some ans -> ans
              | None ->
                  let ans =
                    match aux new_player1 new_player2 with
                    | `One, _ -> true
                    | `Two, _ -> false
                  in
                  cache := Map.set !cache ~key:cache_key ~data:ans ;
                  ans )
            else e1 > e2
          in
          if player1_wins then (
            Queue.enqueue player1 e1 ;
            Queue.enqueue player1 e2 ;
            aux player1 player2 )
          else (
            Queue.enqueue player2 e2 ;
            Queue.enqueue player2 e1 ;
            aux player1 player2 )
      | None, None -> assert false
      | None, _ -> (`Two, player2)
      | _, None -> (`One, player1)
    in
    let _, winner_queue = aux player1 player2 in
    let ans =
      Queue.foldi ~init:0
        ~f:(fun idx acc elem ->
          acc + (elem * (Queue.length winner_queue - idx)))
        winner_queue
    in
    print_endline_int ans
end

include Day.Make (M)

let example = "Player 1:\n9\n2\n6\n3\n1\n\nPlayer 2:\n5\n8\n4\n7\n10"

let%expect_test _ = run example ; [%expect {|
  306
  291 |}]
