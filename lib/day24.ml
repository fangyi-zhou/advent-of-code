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

  let step = function
    | E -> (2, 0)
    | W -> (-2, 0)
    | NW -> (-1, 1)
    | SE -> (1, -1)
    | NE -> (1, 1)
    | SW -> (-1, -1)

  let neighbours (x, y) =
    List.map
      ~f:(fun dir ->
        let dx, dy = step dir in
        (x + dx, y + dy))
      [E; W; NW; SE; NE; SW]

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
      type t = int * int

      let compare (v11, v12) (v21, v22) =
        let cmp1 = Int.compare v11 v21 in
        if cmp1 <> 0 then cmp1 else Int.compare v12 v22

      let sexp_of_t (v1, v2) = Sexp.List [Int.sexp_of_t v1; Int.sexp_of_t v2]
    end

    include M
    include Comparator.Make (M)
  end

  let normalise_trace =
    List.fold ~init:(0, 0) ~f:(fun (x, y) dir ->
        let dx, dy = step dir in
        (x + dx, y + dy))

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

  let part1 traces =
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
    let marked = Map.filter ~f:Fn.id map in
    let ans = Map.length marked in
    print_endline_int ans

  let iter map =
    let keys =
      Set.union_list (module NormalisedTrace)
      @@ List.map
           ~f:(fun key ->
             Set.of_list (module NormalisedTrace) (key :: neighbours key))
           (Map.keys map)
    in
    let map =
      Set.fold
        ~init:(Map.empty (module NormalisedTrace))
        ~f:(fun acc key ->
          let neighbour_black =
            List.count
              ~f:(fun key -> Option.value ~default:false (Map.find map key))
              (neighbours key)
          in
          let current_black =
            Option.value ~default:false (Map.find map key)
          in
          let next_value =
            if current_black then
              if neighbour_black = 0 || neighbour_black > 2 then false
              else true
            else if neighbour_black = 2 then true
            else false
          in
          Map.set acc ~key ~data:next_value)
        keys
    in
    let map = Map.filter ~f:Fn.id map in
    map

  let part2 traces =
    let init =
      List.fold
        ~init:(Map.empty (module NormalisedTrace))
        ~f:(fun acc trace ->
          let trace = normalise_trace trace in
          Map.update acc trace ~f:(function
            | Some marked -> not marked
            | None -> true))
        traces
    in
    let final = Fn.apply_n_times iter ~n:100 init in
    let marked = Map.filter ~f:Fn.id final in
    let ans = Map.length marked in
    print_endline_int ans
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

let%expect_test _ = run example ; [%expect {|
  10
  2208 |}]
