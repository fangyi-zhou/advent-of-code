open! Imports

module M = struct
  type instr = string * int

  type t = instr array

  let parse inputs =
    let lines = String.split ~on:'\n' inputs in
    let parse_instr line =
      match String.split ~on:' ' line with
      | [op; operand] -> (op, Int.of_string operand)
      | _ -> assert false
    in
    let instrs = List.map ~f:parse_instr lines in
    Array.of_list instrs

  let eval imem =
    let rec go pc acc executed_instrs =
      if Set.mem executed_instrs pc then `Loop acc
      else
        let executed_instrs = Set.add executed_instrs pc in
        if pc = Array.length imem then `Terminated acc
        else
          match imem.(pc) with
          | "nop", _ -> go (pc + 1) acc executed_instrs
          | "acc", delta -> go (pc + 1) (acc + delta) executed_instrs
          | "jmp", delta -> go (pc + delta) acc executed_instrs
          | _ -> assert false
    in
    go 0 0 (Set.empty (module Int))

  let part1 imem =
    match eval imem with
    | `Loop ans -> print_endline_int ans
    | _ -> assert false

  let part2 imem =
    let ans =
      Array.fold_until ~init:0
        ~f:(fun idx instr ->
          match instr with
          | "acc", _ -> Continue (idx + 1)
          | "jmp", delta | "nop", delta -> (
              let new_instr =
                ( (if String.equal (fst instr) "jmp" then "nop" else "jmp")
                , delta )
              in
              imem.(idx) <- new_instr ;
              match eval imem with
              | `Loop _ ->
                  imem.(idx) <- instr ;
                  Continue (idx + 1)
              | `Terminated acc -> Stop acc )
          | _ -> assert false)
        ~finish:(fun _ -> assert false)
        imem
    in
    print_endline_int ans
end

include Day.Make (M)

let example =
  "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6"

let%expect_test _ = run example ; [%expect {|
    5
    8|}]
