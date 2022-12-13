open Imports

module M = struct
  type token = IntT of int | AddT | MulT | LParenT | RParenT

  type expr = Int of int | Add of expr * expr | Mult of expr * expr

  type t = token list list

  let rec tokens_of = function
    | "+" -> [AddT]
    | "*" -> [MulT]
    | "(" -> [LParenT]
    | ")" -> [RParenT]
    | int_literal -> (
      match String.chop_prefix ~prefix:"(" int_literal with
      | Some chopped -> LParenT :: tokens_of chopped
      | None -> (
        match String.chop_suffix ~suffix:")" int_literal with
        | None -> [IntT (Int.of_string int_literal)]
        | Some chopped -> tokens_of chopped @ [RParenT] ) )

  let rec show_expr = function
    | Int i -> Int.to_string i
    | Add (e1, e2) ->
        Printf.sprintf "(%s + %s)" (show_expr e1) (show_expr e2)
    | Mult (e1, e2) ->
        Printf.sprintf "(%s * %s)" (show_expr e1) (show_expr e2)

  let parse_expr_equal_prec tokens =
    let rec reduce = function
      | e1 :: e2 :: rest_e, AddT :: rest_ops ->
          reduce (Add (e2, e1) :: rest_e, rest_ops)
      | e1 :: e2 :: rest_e, MulT :: rest_ops ->
          reduce (Mult (e2, e1) :: rest_e, rest_ops)
      | acc, ops -> (acc, ops)
    in
    let rec token (acc, ops) = function
      | [] ->
          let acc, _ = reduce (acc, ops) in
          List.hd_exn acc
      | IntT i :: rest ->
          let acc = Int i :: acc in
          let acc, ops = reduce (acc, ops) in
          token (acc, ops) rest
      | LParenT :: rest -> token (acc, LParenT :: ops) rest
      | RParenT :: rest -> (
          let acc, ops = reduce (acc, ops) in
          match ops with
          | LParenT :: rest_ops -> token (acc, rest_ops) rest
          | _ -> assert false )
      | AddT :: rest ->
          let acc, ops = reduce (acc, ops) in
          token (acc, AddT :: ops) rest
      | MulT :: rest ->
          let acc, ops = reduce (acc, ops) in
          token (acc, MulT :: ops) rest
    in
    token ([], []) tokens

  let parse_expr_nonequal_prec tokens =
    let rec reduce = function
      | e1 :: e2 :: rest_e, AddT :: rest_ops ->
          reduce (Add (e2, e1) :: rest_e, rest_ops)
      | e1 :: e2 :: rest_e, MulT :: rest_ops ->
          reduce (Mult (e2, e1) :: rest_e, rest_ops)
      | acc, ops -> (acc, ops)
    in
    let rec reduce_add = function
      | e1 :: e2 :: rest_e, AddT :: rest_ops ->
          reduce_add (Add (e2, e1) :: rest_e, rest_ops)
      | acc, ops -> (acc, ops)
    in
    let rec token (acc, ops) = function
      | [] ->
          let acc, _ = reduce (acc, ops) in
          List.hd_exn acc
      | IntT i :: rest ->
          let acc = Int i :: acc in
          token (acc, ops) rest
      | LParenT :: rest -> token (acc, LParenT :: ops) rest
      | RParenT :: rest -> (
          let acc, ops = reduce (acc, ops) in
          match ops with
          | LParenT :: rest_ops -> token (acc, rest_ops) rest
          | _ -> assert false )
      | AddT :: rest -> token (acc, AddT :: ops) rest
      | MulT :: rest ->
          let acc, ops = reduce_add (acc, ops) in
          token (acc, MulT :: ops) rest
    in
    token ([], []) tokens

  let parse inputs =
    let lines = String.split ~on:'\n' inputs in
    let tokenise line =
      let tokens = String.split ~on:' ' line in
      let tokens = List.concat_map ~f:tokens_of tokens in
      tokens
    in
    List.map ~f:tokenise lines

  let rec eval = function
    | Int i -> i
    | Add (e1, e2) ->
        let e1 = eval e1 in
        let e2 = eval e2 in
        (* print_endline (Printf.sprintf "%d + %d = %d" e1 e2 (e1 + e2)) ; *)
        e1 + e2
    | Mult (e1, e2) ->
        let e1 = eval e1 in
        let e2 = eval e2 in
        (* print_endline (Printf.sprintf "%d * %d = %d" e1 e2 (e1 * e2)) ; *)
        e1 * e2

  let part1 tokens =
    let exprs = List.map ~f:parse_expr_equal_prec tokens in
    (* List.iter ~f:(fun e -> print_endline (show_expr e)) exprs ; *)
    let ans = List.sum (module Int) ~f:eval exprs in
    print_endline_int ans

  let part2 tokens =
    let exprs = List.map ~f:parse_expr_nonequal_prec tokens in
    (* List.iter ~f:(fun e -> print_endline (show_expr e)) exprs ; *)
    let ans = List.sum (module Int) ~f:eval exprs in
    print_endline_int ans
end

include Day.Make (M)

let%expect_test _ =
  run "1 + (2 * 3) + (4 * (5 + 6))" ;
  [%expect {|
    51
    51 |}]

let%expect_test _ = run "2 * 3 + (4 * 5)" ; [%expect {|
  26
  46 |}]

let%expect_test _ =
  run "5 + (8 * 3 + 9 + 3 * 4 * 3)" ;
  [%expect {|
    437
    1445 |}]

let%expect_test _ =
  run "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" ;
  [%expect {|
    12240
    669060 |}]

let%expect_test _ =
  run "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" ;
  [%expect {|
    13632
    23340 |}]
