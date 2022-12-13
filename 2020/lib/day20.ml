open Imports

module M = struct
  type tiles = char list list

  type t = tiles Map.M(Int).t

  let parse_tile =
    let really_parse_tile = function
      | hd :: tl ->
          let tile_no = String.chop_prefix_exn ~prefix:"Tile " hd in
          let tile_no = String.chop_suffix_exn ~suffix:":" tile_no in
          let tile_no = Int.of_string tile_no in
          (tile_no, List.map ~f:String.to_list tl)
      | _ -> assert false
    in
    function [""] -> None | inputs -> Some (really_parse_tile inputs)

  let parse inputs =
    let lines = String.split ~on:'\n' inputs in
    let parts =
      List.group
        ~break:(fun a b -> String.is_empty a || String.is_empty b)
        lines
    in
    let tiles = List.filter_map ~f:parse_tile parts in
    Map.of_alist_exn (module Int) tiles

  let part1 tiles =
    let edges tile =
      let top = List.hd_exn tile in
      let bot = List.hd_exn (List.rev tile) in
      let left = List.hd_exn (List.transpose_exn tile) in
      let right = List.hd_exn (List.rev (List.transpose_exn tile)) in
      let edges = [top; bot; left; right] in
      let edges = edges @ List.map ~f:List.rev edges in
      let edges = List.map ~f:String.of_char_list edges in
      Set.of_list (module String) edges
    in
    let edges_map = Map.map ~f:edges tiles in
    let corners =
      Map.filter
        ~f:(fun edges_curr ->
          let neighbours =
            Map.count
              ~f:(fun edges_other ->
                not (Set.are_disjoint edges_curr edges_other))
              edges_map
          in
          neighbours = 3
          (* Count include the tile itself *))
        edges_map
    in
    assert (Map.length corners = 4) ;
    let ans =
      Map.fold ~init:1 ~f:(fun ~key ~data:_ acc -> key * acc) corners
    in
    print_endline_int ans

  let part2 _ = ()
end

include Day.Make (M)

let example =
  "Tile 2311:\n\
   ..##.#..#.\n\
   ##..#.....\n\
   #...##..#.\n\
   ####.#...#\n\
   ##.##.###.\n\
   ##...#.###\n\
   .#.#.#..##\n\
   ..#....#..\n\
   ###...#.#.\n\
   ..###..###\n\n\
   Tile 1951:\n\
   #.##...##.\n\
   #.####...#\n\
   .....#..##\n\
   #...######\n\
   .##.#....#\n\
   .###.#####\n\
   ###.##.##.\n\
   .###....#.\n\
   ..#.#..#.#\n\
   #...##.#..\n\n\
   Tile 1171:\n\
   ####...##.\n\
   #..##.#..#\n\
   ##.#..#.#.\n\
   .###.####.\n\
   ..###.####\n\
   .##....##.\n\
   .#...####.\n\
   #.##.####.\n\
   ####..#...\n\
   .....##...\n\n\
   Tile 1427:\n\
   ###.##.#..\n\
   .#..#.##..\n\
   .#.##.#..#\n\
   #.#.#.##.#\n\
   ....#...##\n\
   ...##..##.\n\
   ...#.#####\n\
   .#.####.#.\n\
   ..#..###.#\n\
   ..##.#..#.\n\n\
   Tile 1489:\n\
   ##.#.#....\n\
   ..##...#..\n\
   .##..##...\n\
   ..#...#...\n\
   #####...#.\n\
   #..#.#.#.#\n\
   ...#.#.#..\n\
   ##.#...##.\n\
   ..##.##.##\n\
   ###.##.#..\n\n\
   Tile 2473:\n\
   #....####.\n\
   #..#.##...\n\
   #.##..#...\n\
   ######.#.#\n\
   .#...#.#.#\n\
   .#########\n\
   .###.#..#.\n\
   ########.#\n\
   ##...##.#.\n\
   ..###.#.#.\n\n\
   Tile 2971:\n\
   ..#.#....#\n\
   #...###...\n\
   #.#.###...\n\
   ##.##..#..\n\
   .#####..##\n\
   .#..####.#\n\
   #..#.#..#.\n\
   ..####.###\n\
   ..#.#.###.\n\
   ...#.#.#.#\n\n\
   Tile 2729:\n\
   ...#.#.#.#\n\
   ####.#....\n\
   ..#.#.....\n\
   ....#..#.#\n\
   .##..##.#.\n\
   .#.####...\n\
   ####.#.#..\n\
   ##.####...\n\
   ##..#.##..\n\
   #.##...##.\n\n\
   Tile 3079:\n\
   #.#.#####.\n\
   .#..######\n\
   ..#.......\n\
   ######....\n\
   ####.#..#.\n\
   .#...#.##.\n\
   #.#####.##\n\
   ..#.###...\n\
   ..#.......\n\
   ..#.###..."

let%expect_test _ = run example ; [%expect {|20899048083289|}]
