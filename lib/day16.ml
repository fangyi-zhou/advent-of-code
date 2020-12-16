open Imports

module M = struct
  type ticket = int list

  type range = int * int

  type t = (string * range * range) list * ticket * ticket list

  let parse inputs =
    let parse_int_list line =
      let nums = String.split ~on:',' line in
      List.map ~f:Int.of_string nums
    in
    let parse_range range =
      match String.split ~on:'-' range with
      | [i1; i2] -> (Int.of_string i1, Int.of_string i2)
      | _ -> assert false
    in
    let parse_description line =
      let name, rest = String.rsplit2_exn line ~on:':' in
      match String.split ~on:' ' rest with
      | [_; range1; _; range2] ->
          (name, parse_range range1, parse_range range2)
      | _ -> assert false
    in
    let lines = String.split ~on:'\n' inputs in
    let parts =
      List.group
        ~break:(fun a b -> String.is_empty a || String.is_empty b)
        lines
    in
    match parts with
    | [descriptions; _; [_; your_ticket]; _; (_ :: nearby_tickets)] ->
        let descriptions = List.map ~f:parse_description descriptions in
        let your_ticket = parse_int_list your_ticket in
        let nearby_tickets = List.map ~f:parse_int_list nearby_tickets in
        (descriptions, your_ticket, nearby_tickets)
    | _ -> assert false

  let range_match value (mini, maxi) = value >= mini && value <= maxi

  let field_match (value, (_, range1, range2)) =
    range_match value range1 || range_match value range2

  let rec interleave elem = function
    | [] -> Sequence.singleton [elem]
    | hd :: tl ->
        Sequence.append
          (Sequence.singleton (elem :: hd :: tl))
          (Sequence.map ~f:(fun l -> hd :: l) (interleave elem tl))

  let rec permute = function
    | [] -> Sequence.singleton []
    | hd :: tl -> Sequence.concat_map ~f:(interleave hd) (permute tl)

  let match_any value descriptions =
    List.exists ~f:(fun desc -> field_match (value, desc)) descriptions

  let part1 (descriptions, _, nearby_tickets) =
    let ans =
      List.sum
        (module Int)
        ~f:(fun ticket ->
          List.sum
            (module Int)
            ~f:(fun field ->
              if not (match_any field descriptions) then field else 0)
            ticket)
        nearby_tickets
    in
    print_endline_int ans

  let part2 (descriptions, your_ticket, nearby_tickets) =
    let is_invalid ticket =
      List.for_all ~f:(fun value -> match_any value descriptions) ticket
    in
    let all_tickets = your_ticket :: nearby_tickets in
    let all_tickets = List.filter ~f:is_invalid all_tickets in
    let permutations = permute descriptions in
    let correct_description =
      List.fold ~init:permutations
        ~f:(fun perms ticket ->
          Sequence.filter
            ~f:(fun desc ->
              List.for_all ~f:field_match (List.zip_exn ticket desc))
            perms)
        all_tickets
      |> Sequence.hd_exn
    in
    let ans =
      List.fold ~init:1
        ~f:(fun acc ((desc, _, _), value) ->
          if String.is_prefix ~prefix:"departure" desc then acc * value
          else acc)
        (List.zip_exn correct_description your_ticket)
    in
    print_endline_int ans
end

include Day.Make (M)

let example =
  "class: 1-3 or 5-7\n\
   row: 6-11 or 33-44\n\
   seat: 13-40 or 45-50\n\n\
   your ticket:\n\
   7,1,14\n\n\
   nearby tickets:\n\
   7,3,47\n\
   40,4,50\n\
   55,2,20\n\
   38,6,12"

let%expect_test _ =
  run ~only_part1:true example ;
  [%expect {| 71 |}]
