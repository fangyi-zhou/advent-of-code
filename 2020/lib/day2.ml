open! Imports

module M = struct
  type t = (int * int * char * string) list

  let parse inputs =
    let f line =
      let splitted = String.split_on_chars ~on:[':'; ' '; '-'] line in
      match splitted with
      | mini :: maxi :: letter :: _ :: pwd :: _ ->
          (Int.of_string mini, Int.of_string maxi, letter.[0], pwd)
      | _ -> assert false
    in
    let lines = String.split inputs ~on:'\n' in
    List.map ~f lines

  let part1 entries =
    let ans =
      List.count
        ~f:(fun (mini, maxi, letter, pwd) ->
          let count = String.count ~f:(Char.( = ) letter) pwd in
          count >= mini && count <= maxi )
        entries
    in
    print_endline_int ans

  let part2 entries =
    let ans =
      List.count
        ~f:(fun (pos1, pos2, letter, pwd) ->
          not
          @@ Bool.equal
               (Char.equal pwd.[pos1 - 1] letter)
               (Char.equal pwd.[pos2 - 1] letter) )
        entries
    in
    print_endline_int ans
end

include Day.Make (M)

let example = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"

let%expect_test _ = run example ; [%expect {|
    2
    1 |}]
