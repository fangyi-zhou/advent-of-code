open Imports

module M = struct
  type dir = E | SE | SW | W | NW | NE

  let opposite = function
    | E -> W
    | W -> E
    | NW -> SE
    | SE -> NW
    | NE -> SW
    | SW -> NE

  type trace = dir list

  module Dir = struct
    module M = struct
      type t = dir

      let compare = Poly.compare

      let sexp_of_t = function
        | E -> Sexp.Atom "E"
        | SE -> Sexp.Atom "SE"
        | SW -> Sexp.Atom "SW"
        | W -> Sexp.Atom "W"
        | NW -> Sexp.Atom "NW"
        | NE -> Sexp.Atom "NE"
    end

    include M
    include Comparator.Make (M)
  end

  module NormalisedTrace = struct
    module M = struct
      type t = int Map.M(Dir).t

      let compare = Map.compare_direct compare

      let sexp_of_t = Map.sexp_of_m__t (module Dir) Int.sexp_of_t
    end

    include M
    include Comparator.Make (M)
  end

  let normalise_trace =
    List.fold
      ~init:(Map.empty (module Dir))
      ~f:(fun acc dir ->
        let opposite_dir = opposite dir in
        match Map.find acc opposite_dir with
        | Some 1 -> Map.remove acc opposite_dir
        | Some num -> Map.set acc ~key:opposite_dir ~data:(num - 1)
        | None ->
            Map.update acc dir ~f:(function
              | Some num -> num + 1
              | None -> 1))

  type t = trace list

  let parse_trace str =
    let rec aux acc = function
      | [] -> List.rev acc
      | 'e' :: rest -> aux (E :: acc) rest
      | 'w' :: rest -> aux (W :: acc) rest
      | 'n' :: 'w' :: rest -> aux (NW :: acc) rest
      | 'n' :: 'e' :: rest -> aux (NE :: acc) rest
      | 's' :: 'w' :: rest -> aux (SW :: acc) rest
      | 's' :: 'e' :: rest -> aux (SE :: acc) rest
      | _ -> assert false
    in
    aux [] (String.to_list str)

  let parse inputs =
    let lines = String.split ~on:'\n' inputs in
    List.map ~f:parse_trace lines

  let _part1 traces =
    let map =
      List.fold
        ~init:(Map.empty (module NormalisedTrace))
        ~f:(fun acc trace ->
          let trace = normalise_trace trace in
          Map.update acc trace ~f:(function
            | Some marked -> not marked
            | None -> true))
        traces
    in
    print_s (Map.sexp_of_m__t (module NormalisedTrace) Bool.sexp_of_t map) ;
    let marked = Map.filter ~f:Fn.id map in
    let ans = Map.length marked in
    print_endline_int ans

  let part1 _ = ()

  let part2 _ = ()
end

include Day.Make (M)

let example =
  "sesenwnenenewseeswwswswwnenewsewsw\n\
   neeenesenwnwwswnenewnwwsewnenwseswesw\n\
   seswneswswsenwwnwse\n\
   nwnwneseeswswnenewneswwnewseswneseene\n\
   swweswneswnenwsewnwneneseenw\n\
   eesenwseswswnenwswnwnwsewwnwsene\n\
   sewnenenenesenwsewnenwwwse\n\
   wenwwweseeeweswwwnwwe\n\
   wsweesenenewnwwnwsenewsenwwsesesenwne\n\
   neeswseenwwswnwswswnw\n\
   nenwswwsewswnenenewsenwsenwnesesenew\n\
   enewnwewneswsewnwswenweswnenwsenwsw\n\
   sweneswneswneneenwnewenewwneswswnese\n\
   swwesenesewenwneswnwwneseswwne\n\
   enesenwswwswneneswsenwnewswseenwsese\n\
   wnwnesenesenenwwnenwsewesewsesesew\n\
   nenewswnwewswnenesenwnesewesw\n\
   eneswnwswnwsenenwnwnwwseeswneewsenese\n\
   neswnwewnwnwseenwseesewsenwsweewe\n\
   wseweeenwnesenwwwswnew"

let%expect_test _ = run example ; [%expect {||}]
