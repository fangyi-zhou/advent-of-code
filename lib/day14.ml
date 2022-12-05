open Imports

module M = struct
  type instr = MemSet of int * int | MaskSet of (int * int * int list)

  type t = instr list

  let parse inputs =
    let lines = String.split ~on:'\n' inputs in
    let parse_mask mask =
      let zero_mask =
        String.map ~f:(fun ch -> if Char.equal ch '0' then '0' else '1') mask
      in
      let one_mask =
        String.map ~f:(fun ch -> if Char.equal ch '1' then '1' else '0') mask
      in
      let x_bits =
        List.filter_mapi
          ~f:(fun idx ch -> if Char.equal ch 'X' then Some idx else None)
          (String.to_list_rev mask)
      in
      let zero_mask = Int.of_string ("0b" ^ zero_mask) in
      let one_mask = Int.of_string ("0b" ^ one_mask) in
      (zero_mask, one_mask, x_bits)
    in
    let parse_instr line =
      let splitted = String.split ~on:'=' line in
      match splitted with
      | [lval; rval] -> (
          let lval = String.strip lval in
          let rval = String.strip rval in
          match lval with
          | "mask" -> MaskSet (parse_mask rval)
          | mem ->
              let operand = Int.of_string rval in
              let addr =
                Int.of_string
                  (String.chop_suffix_exn ~suffix:"]"
                     (String.chop_prefix_exn ~prefix:"mem[" mem) )
              in
              MemSet (addr, operand) )
      | _ -> assert false
    in
    List.map ~f:parse_instr lines

  let part1 instrs =
    let f (mask, mem) = function
      | MaskSet mask -> (mask, mem)
      | MemSet (addr, value) ->
          let zero_mask, one_mask, _ = mask in
          let value = value land zero_mask in
          let value = value lor one_mask in
          (mask, Map.update mem addr ~f:(fun _ -> value))
    in
    let _, mem =
      List.fold ~f ~init:((0, 0, []), Map.empty (module Int)) instrs
    in
    let sum = Map.fold ~init:0 ~f:(fun ~key:_ ~data acc -> acc + data) mem in
    print_endline_int sum

  let make_addrs addr x_bits =
    let rec aux acc = function
      | [] -> acc
      | hd :: tl ->
          let f addr =
            let with_zero = addr land lnot (1 lsl hd) in
            let with_one = addr lor (1 lsl hd) in
            [with_zero; with_one]
          in
          let acc = List.concat_map ~f acc in
          aux acc tl
    in
    aux [addr] x_bits

  let part2 instrs =
    let f (mask, mem) = function
      | MaskSet mask -> (mask, mem)
      | MemSet (addr, value) ->
          let _zero_mask, one_mask, x_bits = mask in
          let addr = addr lor one_mask in
          let addrs = make_addrs addr x_bits in
          let mem =
            List.fold ~init:mem ~f:(Map.update ~f:(fun _ -> value)) addrs
          in
          (mask, mem)
    in
    let _, mem =
      List.fold ~f ~init:((0, 0, []), Map.empty (module Int)) instrs
    in
    let sum = Map.fold ~init:0 ~f:(fun ~key:_ ~data acc -> acc + data) mem in
    print_endline_int sum
end

include Day.Make (M)

let example1 =
  "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
   mem[8] = 11\n\
   mem[7] = 101\n\
   mem[8] = 0"

let%expect_test _ =
  run example1 ~only_part1:true ;
  [%expect {| 165 |}]

let example2 =
  "mask = 000000000000000000000000000000X1001X\n\
   mem[42] = 100\n\
   mask = 00000000000000000000000000000000X0XX\n\
   mem[26] = 1"

let%expect_test _ =
  run example2 ~only_part2:true ;
  [%expect {| 208 |}]
