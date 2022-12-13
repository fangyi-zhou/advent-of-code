open Imports

module M = struct
  type abs_dir = N | W | S | E

  let move_delta = function
    | N -> (0, 1)
    | S -> (0, -1)
    | E -> (1, 0)
    | W -> (-1, 0)

  type rel_dir = L | R

  let turn_right_dir = function N -> E | E -> S | S -> W | W -> N

  let turn_right_point (x, y) = (y, -x)

  type instr =
    | Move of abs_dir * int
    | Forward of int
    | Rotate of rel_dir * int

  type t = instr list

  let parse inputs =
    let lines = String.split ~on:'\n' inputs in
    let f line =
      let operand = String.sub line ~pos:1 ~len:(String.length line - 1) in
      let operand = Int.of_string operand in
      match line.[0] with
      | 'N' -> Move (N, operand)
      | 'W' -> Move (W, operand)
      | 'S' -> Move (S, operand)
      | 'E' -> Move (E, operand)
      | 'F' -> Forward operand
      | 'L' -> Rotate (L, operand / 90)
      | 'R' -> Rotate (R, operand / 90)
      | _ -> assert false
    in
    List.map ~f lines

  let move_dir direction (x, y) steps =
    let dx, dy = move_delta direction in
    (x + (steps * dx), y + (steps * dy))

  let rotate_dir rel_dir steps =
    match rel_dir with
    | R -> Fn.apply_n_times ~n:(steps % 4) turn_right_dir
    | L -> Fn.apply_n_times ~n:(4 - (steps % 4)) turn_right_dir

  let rotate_point rel_dir steps =
    match rel_dir with
    | R -> Fn.apply_n_times ~n:(steps % 4) turn_right_point
    | L -> Fn.apply_n_times ~n:(4 - (steps % 4)) turn_right_point

  let part1 instrs =
    let init = (E, (0, 0)) in
    let f (facing, pos) = function
      | Forward steps -> (facing, move_dir facing pos steps)
      | Move (abs_dir, steps) -> (facing, move_dir abs_dir pos steps)
      | Rotate (rel_dir, steps) -> (rotate_dir rel_dir steps facing, pos)
    in
    let _, (x, y) = List.fold ~init ~f instrs in
    let ans = Int.abs x + Int.abs y in
    print_endline_int ans

  let part2 instrs =
    let init = ((10, 1), (0, 0)) in
    let f (waypoint, pos) = function
      | Forward steps ->
          let dx, dy = waypoint in
          let x, y = pos in
          (waypoint, (x + (steps * dx), y + (steps * dy)))
      | Move (abs_dir, steps) -> (move_dir abs_dir waypoint steps, pos)
      | Rotate (rel_dir, steps) -> (rotate_point rel_dir steps waypoint, pos)
    in
    let _, (x, y) = List.fold ~init ~f instrs in
    let ans = Int.abs x + Int.abs y in
    print_endline_int ans
end

include Day.Make (M)

let example = "F10\nN3\nF7\nR90\nF11"

let%expect_test _ = run example ; [%expect {|
  25
  286|}]
