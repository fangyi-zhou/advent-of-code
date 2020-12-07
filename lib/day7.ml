open! Base
open! Stdio

module M = struct
  type t = string list Map.M(String).t * (int * string) list Map.M(String).t

  let combine s1 s2 = s1 ^ " " ^ s2

  let parse inputs =
    let lines = String.split ~on:'\n' inputs in
    let f acc line =
      let words = String.split ~on:' ' line in
      let update acc value key =
        Map.update acc key ~f:(function
          | None -> [value]
          | Some rest -> value :: rest)
      in
      let rec process_contents ((acc1, acc2) as acc) outer = function
        | [] | ["no"; "other"; "bags."] -> acc
        | num :: col1 :: col2 :: _bags :: rest ->
            let num = Int.of_string num in
            let inner = combine col1 col2 in
            let acc =
              (update acc1 outer inner, update acc2 (num, inner) outer)
            in
            process_contents acc outer rest
        | _ -> assert false
      in
      match words with
      | col1 :: col2 :: _bags :: _contain :: rest ->
          let outer = combine col1 col2 in
          process_contents acc outer rest
      | _ -> assert false
    in
    List.fold
      ~init:(Map.empty (module String), Map.empty (module String))
      ~f lines

  let part1 (deps, _) =
    let init = Set.singleton (module String) "shiny gold" in
    let rec fixpoint curr =
      let updated = ref false in
      let f acc elem =
        let nexts = Option.value ~default:[] (Map.find deps elem) in
        List.fold ~init:acc
          ~f:(fun acc next ->
            if Set.mem acc next then acc
            else (
              updated := true ;
              Set.add acc next ))
          nexts
      in
      let next = Set.fold ~init:curr ~f curr in
      if !updated then fixpoint next else next
    in
    let fp = fixpoint init in
    let ans = Set.length fp - 1 (* Remove shiny gold *) in
    print_endline (Int.to_string ans)

  let part2 (_, deps) =
    let cache = ref (Map.empty (module String)) in
    let rec cost color =
      match Map.find !cache color with
      | Some v -> v
      | None ->
          let nexts = Option.value ~default:[] (Map.find deps color) in
          let ans =
            1
            + List.sum
                (module Int)
                ~f:(fun (amount, next_color) -> amount * cost next_color)
                nexts
          in
          cache := Map.add_exn !cache ~key:color ~data:ans ;
          ans
    in
    let ans = cost "shiny gold" - 1 (* remove shiny gold *) in
    print_endline (Int.to_string ans)
end

include Day.Make (M)

let example_1 =
  "light red bags contain 1 bright white bag, 2 muted yellow bags.\n\
   dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n\
   bright white bags contain 1 shiny gold bag.\n\
   muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n\
   shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n\
   dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n\
   vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n\
   faded blue bags contain no other bags.\n\
   dotted black bags contain no other bags."

let example_2 =
  "shiny gold bags contain 2 dark red bags.\n\
   dark red bags contain 2 dark orange bags.\n\
   dark orange bags contain 2 dark yellow bags.\n\
   dark yellow bags contain 2 dark green bags.\n\
   dark green bags contain 2 dark blue bags.\n\
   dark blue bags contain 2 dark violet bags.\n\
   dark violet bags contain no other bags."

let%expect_test _ = run example_1 ; [%expect {|
      4
      32|}]

let%expect_test _ =
  run ~only_part2:true example_2 ;
  [%expect {|126|}]
