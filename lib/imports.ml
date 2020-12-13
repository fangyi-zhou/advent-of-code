include Base
include Stdio

let print_endline_int i = print_endline (Int.to_string i)

module String = struct
  include String

  let iteri ~f str = ignore (fold ~init:0 ~f:(fun i x -> f i x ; i + 1) str)
end

module List = struct
  include List

  let product = List.fold ~init:1 ~f:( * )
end
