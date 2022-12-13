open Imports

module M = struct
  type rule =
    | Chr of char
    | Alt of rule list
    | Concat of rule list
    | Ref of int

  type t = rule Map.M(Int).t * string list

  let parse_rule input =
    match String.split ~on:':' input with
    | [entry_no; rule] ->
        let entry_no = Int.of_string entry_no in
        let rec aux rule =
          let rule = String.strip rule in
          if Char.equal rule.[0] '\"' then Chr rule.[1]
          else
            match String.split ~on:'|' rule with
            | [singleton] -> (
                let singleton = String.strip singleton in
                match String.split ~on:' ' singleton with
                | [singleton] -> Ref (Int.of_string singleton)
                | concats -> Concat (List.map ~f:aux concats) )
            | alts -> Alt (List.map ~f:aux alts)
        in
        (entry_no, aux rule)
    | _ -> assert false

  let parse inputs =
    let lines = String.split ~on:'\n' inputs in
    let parts =
      List.group
        ~break:(fun a b -> String.is_empty a || String.is_empty b)
        lines
    in
    match parts with
    | [rules; _; entries] ->
        let rules = List.map ~f:parse_rule rules in
        (Map.of_alist_exn (module Int) rules, entries)
    | _ -> assert false

  let rec match_rule refs str = function
    | Ref ref ->
        let rule = Map.find_exn refs ref in
        match_rule refs str rule
    | Chr ch -> (
      match str with
      | hd :: tl when Char.equal ch hd -> Some [tl]
      | _ -> None )
    | Alt alts -> (
      match List.concat @@ List.filter_map ~f:(match_rule refs str) alts with
      | [] -> None
      | tls -> Some tls )
    | Concat rules -> (
      match
        List.fold ~init:[str]
          ~f:(fun acc rule ->
            List.concat
            @@ List.filter_map ~f:(fun s -> match_rule refs s rule) acc )
          rules
      with
      | [] -> None
      | tls -> Some tls )

  let part1 (refs, entries) =
    let ans =
      List.count
        ~f:(fun str ->
          let str = String.to_list str in
          match match_rule refs str (Ref 0) with
          | Some [[]] -> true
          | _ -> false )
        entries
    in
    print_endline_int ans

  let part2 (refs, entries) =
    let refs =
      Map.set refs ~key:8 ~data:(Alt [Ref 42; Concat [Ref 42; Ref 8]])
    in
    let refs =
      Map.set refs ~key:11
        ~data:(Alt [Concat [Ref 42; Ref 31]; Concat [Ref 42; Ref 11; Ref 31]])
    in
    let ans =
      List.count
        ~f:(fun str ->
          let str = String.to_list str in
          match match_rule refs str (Ref 0) with
          | Some endings -> List.exists ~f:List.is_empty endings
          | _ -> false )
        entries
    in
    print_endline_int ans
end

include Day.Make (M)

let example1 =
  "0: 4 1 5\n\
   1: 2 3 | 3 2\n\
   2: 4 4 | 5 5\n\
   3: 4 5 | 5 4\n\
   4: \"a\"\n\
   5: \"b\"\n\n\
   ababbb\n\
   bababa\n\
   abbbab\n\
   aaabbb\n\
   aaaabbb"

let%expect_test _ =
  run ~only_part1:true example1 ;
  [%expect {| 2 |}]

let example2 =
  "42: 9 14 | 10 1\n\
   9: 14 27 | 1 26\n\
   10: 23 14 | 28 1\n\
   1: \"a\"\n\
   11: 42 31\n\
   5: 1 14 | 15 1\n\
   19: 14 1 | 14 14\n\
   12: 24 14 | 19 1\n\
   16: 15 1 | 14 14\n\
   31: 14 17 | 1 13\n\
   6: 14 14 | 1 14\n\
   2: 1 24 | 14 4\n\
   0: 8 11\n\
   13: 14 3 | 1 12\n\
   15: 1 | 14\n\
   17: 14 2 | 1 7\n\
   23: 25 1 | 22 14\n\
   28: 16 1\n\
   4: 1 1\n\
   20: 14 14 | 1 15\n\
   3: 5 14 | 16 1\n\
   27: 1 6 | 14 18\n\
   14: \"b\"\n\
   21: 14 1 | 1 14\n\
   25: 1 1 | 1 14\n\
   22: 14 14\n\
   8: 42\n\
   26: 14 22 | 1 20\n\
   18: 15 15\n\
   7: 14 5 | 1 21\n\
   24: 14 1\n\n\
   abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa\n\
   bbabbbbaabaabba\n\
   babbbbaabbbbbabbbbbbaabaaabaaa\n\
   aaabbbbbbaaaabaababaabababbabaaabbababababaaa\n\
   bbbbbbbaaaabbbbaaabbabaaa\n\
   bbbababbbbaaaaaaaabbababaaababaabab\n\
   ababaaaaaabaaab\n\
   ababaaaaabbbaba\n\
   baabbaaaabbaaaababbaababb\n\
   abbbbabbbbaaaababbbbbbaaaababb\n\
   aaaaabbaabaaaaababaa\n\
   aaaabbaaaabbaaa\n\
   aaaabbaabbaaaaaaabbbabbbaaabbaabaaa\n\
   babaaabbbaaabaababbaabababaaab\n\
   aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"

let%expect_test _ = run example2 ; [%expect {|
  3
  12 |}]
