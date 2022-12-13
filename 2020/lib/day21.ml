open Imports

module M = struct
  type t = (string list * string list) list

  let parse inputs =
    let lines = String.split ~on:'\n' inputs in
    let parse_line line =
      match String.split ~on:'(' line with
      | [ingredients; allergens] ->
          let ingredients = String.strip ingredients in
          let ingredients = String.split ~on:' ' ingredients in
          let allergens =
            String.chop_prefix_exn ~prefix:"contains " allergens
          in
          let allergens = String.chop_suffix_exn ~suffix:")" allergens in
          let allergens =
            List.map ~f:String.strip (String.split ~on:',' allergens)
          in
          (ingredients, allergens)
      | _ -> assert false
    in
    List.map ~f:parse_line lines

  let find_allergies entries =
    let allergies =
      List.fold
        ~init:(Map.empty (module String))
        ~f:(fun acc (ingredients, allergens) ->
          let ingredients = Set.of_list (module String) ingredients in
          List.fold ~init:acc
            ~f:(fun acc allergen ->
              Map.update acc allergen ~f:(function
                | None -> ingredients
                | Some existing -> Set.inter existing ingredients ) )
            allergens )
        entries
    in
    let iter allergies =
      let singletons, others =
        Map.partition_tf ~f:(fun is -> Set.length is = 1) allergies
      in
      let singleton_allergens =
        Set.union_list (module String) (Map.data singletons)
      in
      let others =
        Map.map ~f:(fun is -> Set.diff is singleton_allergens) others
      in
      Map.merge_skewed singletons others ~combine:(fun ~key:_ ->
          assert false )
    in
    let rec fix iter acc =
      let next = iter acc in
      if Map.equal Set.equal next acc then next else fix iter next
    in
    let allergies = fix iter allergies in
    allergies

  let part1 entries =
    let ingredients =
      Set.union_list
        (module String)
        (List.map ~f:(fun (is, _) -> Set.of_list (module String) is) entries)
    in
    let allergies = find_allergies entries in
    (* Map.iteri
     *  ~f:(fun ~key ~data ->
     *    print_endline
     *      (Printf.sprintf "%s: %s" key
     *         (String.concat ~sep:", " (Set.to_list data))))
     * allergies ;
     *)
    let non_allergens =
      Set.diff ingredients
        (Set.union_list (module String) (Map.data allergies))
    in
    let ans =
      List.sum
        (module Int)
        ~f:(fun (is, _) -> List.count ~f:(Set.mem non_allergens) is)
        entries
    in
    print_endline_int ans

  let part2 entries =
    let allergies = find_allergies entries in
    let ans =
      String.concat ~sep:","
        (List.map
           ~f:(fun (_, a) -> Set.choose_exn a)
           (Map.to_alist allergies) )
    in
    print_endline ans
end

include Day.Make (M)

let example =
  "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\n\
   trh fvjkl sbzzf mxmxvkd (contains dairy)\n\
   sqjhc fvjkl (contains soy)\n\
   sqjhc mxmxvkd sbzzf (contains fish)"

let%expect_test _ = run example ; [%expect {|
  5
  mxmxvkd,sqjhc,fvjkl|}]
