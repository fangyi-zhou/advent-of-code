open Imports

module M = struct
  module Vec2 = struct
    module M = struct
      type t = int * int

      let compare (v11, v12) (v21, v22) =
        let cmp1 = compare v11 v21 in
        if cmp1 <> 0 then cmp1 else compare v12 v22

      let sexp_of_t (v1, v2) = Sexp.List (List.map ~f:Int.sexp_of_t [v1; v2])
    end

    include M
    include Comparator.Make (M)

    let of_vec2 x = x
  end

  module Vec3 = struct
    module M = struct
      type t = int * int * int

      let compare (v11, v12, v13) (v21, v22, v23) =
        let cmp1 = compare v11 v21 in
        if cmp1 <> 0 then cmp1
        else
          let cmp2 = compare v12 v22 in
          if cmp2 <> 0 then cmp2 else compare v13 v23

      let sexp_of_t (v1, v2, v3) =
        Sexp.List (List.map ~f:Int.sexp_of_t [v1; v2; v3])
    end

    include M
    include Comparator.Make (M)

    let of_vec2 (x, y) = (x, y, 0)
  end

  module Vec4 = struct
    module M = struct
      type t = int * int * int * int

      let compare (v11, v12, v13, v14) (v21, v22, v23, v24) =
        let cmp1 = compare v11 v21 in
        if cmp1 <> 0 then cmp1
        else
          let cmp2 = compare v12 v22 in
          if cmp2 <> 0 then cmp2
          else
            let cmp3 = compare v13 v23 in
            if cmp3 <> 0 then cmp3 else compare v14 v24

      let sexp_of_t (v1, v2, v3, v4) =
        Sexp.List (List.map ~f:Int.sexp_of_t [v1; v2; v3; v4])
    end

    include M
    include Comparator.Make (M)

    let of_vec2 (x, y) = (x, y, 0, 0)
  end

  type t = bool Map.M(Vec2).t * Vec2.t * Vec2.t

  let parse inputs =
    let lines = String.split ~on:'\n' inputs in
    let dimy = List.length lines in
    let dimx = String.length (List.hd_exn lines) in
    let map =
      List.foldi
        ~init:(Map.empty (module Vec2))
        ~f:(fun y acc ->
          String.foldi ~init:acc ~f:(fun x acc ch ->
              Map.add_exn acc ~key:(x, y) ~data:(Char.equal ch '#')))
        lines
    in
    (map, (0, 0), (dimx, dimy))

  let vec3_input_of_vec2_input (map, small, big) =
    let map =
      Map.fold
        ~init:(Map.empty (module Vec3))
        ~f:(fun ~key ~data acc ->
          Map.add_exn ~key:(Vec3.of_vec2 key) ~data acc)
        map
    in
    let small = Vec3.of_vec2 small in
    let big = Vec3.of_vec2 big in
    (map, small, big)

  let vec4_input_of_vec2_input (map, small, big) =
    let map =
      Map.fold
        ~init:(Map.empty (module Vec4))
        ~f:(fun ~key ~data acc ->
          Map.add_exn ~key:(Vec4.of_vec2 key) ~data acc)
        map
    in
    let small = Vec4.of_vec2 small in
    let big = Vec4.of_vec2 big in
    (map, small, big)

  let part1 inputs =
    let inputs = vec3_input_of_vec2_input inputs in
    let transform map (x, y, z) =
      let active = Option.value ~default:false (Map.find map (x, y, z)) in
      let count = ref 0 in
      for dx = -1 to 1 do
        for dy = -1 to 1 do
          for dz = -1 to 1 do
            if dx = 0 && dy = 0 && dz = 0 then ()
            else
              match Map.find map (x + dx, y + dy, z + dz) with
              | Some true -> count := !count + 1
              | _ -> ()
          done
        done
      done ;
      match (active, !count) with
      | true, 2 | true, 3 -> true
      | true, _ -> false
      | false, 3 -> true
      | false, _ -> false
    in
    let iter_once (map, (x1, y1, z1), (x2, y2, z2)) =
      let x1, y1, z1 = (x1 - 1, y1 - 1, z1 - 1) in
      let x2, y2, z2 = (x2 + 1, y2 + 1, z2 + 1) in
      let map_ref = ref (Map.empty (module Vec3)) in
      for x = x1 to x2 do
        for y = y1 to y2 do
          for z = z1 to z2 do
            let key = (x, y, z) in
            let data = transform map key in
            map_ref := Map.add_exn ~data ~key !map_ref
          done
        done
      done ;
      (!map_ref, (x1, y1, z1), (x2, y2, z2))
    in
    let map, _, _ = Fn.apply_n_times ~n:6 iter_once inputs in
    let ans = Map.count ~f:Fn.id map in
    print_endline_int ans

  let part2 inputs =
    let inputs = vec4_input_of_vec2_input inputs in
    let transform map (x, y, z, w) =
      let active = Option.value ~default:false (Map.find map (x, y, z, w)) in
      let count = ref 0 in
      for dx = -1 to 1 do
        for dy = -1 to 1 do
          for dz = -1 to 1 do
            for dw = -1 to 1 do
              if dx = 0 && dy = 0 && dz = 0 && dw = 0 then ()
              else
                match Map.find map (x + dx, y + dy, z + dz, w + dw) with
                | Some true -> count := !count + 1
                | _ -> ()
            done
          done
        done
      done ;
      match (active, !count) with
      | true, 2 | true, 3 -> true
      | true, _ -> false
      | false, 3 -> true
      | false, _ -> false
    in
    let iter_once (map, (x1, y1, z1, w1), (x2, y2, z2, w2)) =
      let x1, y1, z1, w1 = (x1 - 1, y1 - 1, z1 - 1, w1 - 1) in
      let x2, y2, z2, w2 = (x2 + 1, y2 + 1, z2 + 1, w2 + 1) in
      let map_ref = ref (Map.empty (module Vec4)) in
      for x = x1 to x2 do
        for y = y1 to y2 do
          for z = z1 to z2 do
            for w = w1 to w2 do
              let key = (x, y, z, w) in
              let data = transform map key in
              map_ref := Map.add_exn ~data ~key !map_ref
            done
          done
        done
      done ;
      (!map_ref, (x1, y1, z1, w1), (x2, y2, z2, w2))
    in
    let map, _, _ = Fn.apply_n_times ~n:6 iter_once inputs in
    let ans = Map.count ~f:Fn.id map in
    print_endline_int ans
end

include Day.Make (M)

let example = ".#.\n..#\n###"

let%expect_test _ = run example ; [%expect {|
  112
  848 |}]
