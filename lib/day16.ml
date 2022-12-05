open Imports

module M = struct
  type ticket = int list

  type range = int * int

  type description = string * (range * range)

  type t = description list * ticket * ticket list

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
          (name, (parse_range range1, parse_range range2))
      | _ -> assert false
    in
    let lines = String.split ~on:'\n' inputs in
    let parts =
      List.group
        ~break:(fun a b -> String.is_empty a || String.is_empty b)
        lines
    in
    match parts with
    | [descriptions; _; [_; your_ticket]; _; _ :: nearby_tickets] ->
        let descriptions = List.map ~f:parse_description descriptions in
        let your_ticket = parse_int_list your_ticket in
        let nearby_tickets = List.map ~f:parse_int_list nearby_tickets in
        (descriptions, your_ticket, nearby_tickets)
    | _ -> assert false

  let range_match value (mini, maxi) = value >= mini && value <= maxi

  let field_match (value, (_, (range1, range2))) =
    range_match value range1 || range_match value range2

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
              if not (match_any field descriptions) then field else 0 )
            ticket )
        nearby_tickets
    in
    print_endline_int ans

  let take_one lst full_lst =
    let lst = Set.to_list lst in
    List.map ~f:(fun v -> (v, Set.remove full_lst v)) lst

  module Description = struct
    module M = struct
      type t = description

      let compare (v1, _) (v2, _) = String.compare v1 v2

      let sexp_of_t (v1, ((v2, v3), (v4, v5))) =
        Sexp.List
          [ String.sexp_of_t v1
          ; Int.sexp_of_t v2
          ; Int.sexp_of_t v3
          ; Int.sexp_of_t v4
          ; Int.sexp_of_t v5 ]
    end

    include M
    include Comparator.Make (M)
  end

  let part2 (descriptions, your_ticket, nearby_tickets) =
    let is_invalid ticket =
      List.for_all ~f:(fun value -> match_any value descriptions) ticket
    in
    let all_tickets = your_ticket :: nearby_tickets in
    let all_tickets = List.filter ~f:is_invalid all_tickets in
    let all_tickets = List.transpose_exn all_tickets in
    let filter_descs col descs =
      List.filter
        ~f:(fun desc -> List.for_all ~f:(fun v -> field_match (v, desc)) col)
        descs
    in
    let possible_descs =
      List.map ~f:(fun col -> filter_descs col descriptions) all_tickets
    in
    let possible_descs, tickets =
      let zipped = List.zip_exn possible_descs all_tickets in
      let sorted =
        List.sort
          ~compare:(fun (a, _) (b, _) ->
            Int.compare (List.length a) (List.length b) )
          zipped
      in
      List.unzip sorted
    in
    let correct_description =
      let rec aux determined possible_descs descs =
        match possible_descs with
        | [] -> Some (List.rev determined)
        | col :: rest_cols -> (
            let possible_descs = Set.of_list (module Description) col in
            let possible_descs = Set.inter possible_descs descs in
            if Set.is_empty possible_descs then None
            else
              match
                List.filter_map
                  ~f:(fun (determined_desc, descs) ->
                    aux
                      (determined_desc :: determined)
                      rest_cols
                      (Set.remove descs determined_desc) )
                  (take_one possible_descs descs)
              with
              | [] -> None
              | [ans] -> Some ans
              | _ -> assert false )
      in
      let descs = Set.of_list (module Description) descriptions in
      Option.value_exn (aux [] possible_descs descs)
    in
    let your_ticket = List.hd_exn (List.transpose_exn tickets) in
    let ans =
      List.fold ~init:1
        ~f:(fun acc ((desc, _), value) ->
          if String.is_prefix ~prefix:"departure" desc then acc * value
          else acc )
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
