open! Core

type t =
  | None
  | Wall
  | Start
  | End
[@@deriving compare, sexp, hash]

let position_to_string (position : int * int) : string =
  let x, y = position in
  Int.to_string x ^ ", " ^ Int.to_string y
;;

let position_val ~maze_array ~row ~col : char =
  List.nth_exn (List.nth_exn maze_array row) col
;;

let txt_to_array resource : char list list =
  let contents = In_channel.read_lines (File_path.to_string resource) in
  List.map contents ~f:(fun x -> String.to_list x)
;;

let array_height maze_array = List.length maze_array
let array_length maze_array = List.length (List.nth_exn maze_array 0)

let find_start_pos maze_array =
  let found = ref false in
  let row = ref 0 in
  let col = ref 0 in
  while not !found do
    if Char.equal (position_val ~maze_array ~row:!row ~col:!col) 'S'
    then found.contents <- true
    else (
      col.contents <- !col + 1;
      if col.contents = array_length maze_array
      then (
        col.contents <- 0;
        row.contents <- !row + 1)
      else ())
  done;
  !row, !col
;;

let find_neighbors ~(maze_array : char list list) ~position ~length ~height
  : (int * int) list
  =
  let (x : int), (y : int) = position in
  let neighbor_list = [ x + 1, y; x - 1, y; x, y + 1; x, y - 1 ] in
  List.filter neighbor_list ~f:(fun (a, b) ->
    a >= 0 && a < height && b >= 0 && b < length)
  |> List.filter ~f:(fun (a, b) ->
    not (Char.equal (position_val ~maze_array ~row:a ~col:b) '#'))
;;

let rec solve
  ~(maze_array : char list list)
  ~position
  ~length
  ~height
  ~visited
  : (int * int) list
  =
  let (row : int), (col : int) = position in
  if Char.equal (position_val ~maze_array ~row ~col) 'E'
  then [ position ]
  else (
    let neighbors = find_neighbors ~maze_array ~position ~length ~height in
    if List.is_empty neighbors
    then []
    else
      List.concat_map neighbors ~f:(fun neighbor ->
        match
          Hash_set.find visited ~f:(fun y ->
            String.equal (position_to_string neighbor) y)
        with
        | Some _ -> []
        | _ ->
          Hash_set.add visited (position_to_string neighbor);
          let neighbor_path =
            solve ~maze_array ~position:neighbor ~length ~height ~visited
          in
          (match neighbor_path with
           | [] -> []
           | _ -> [ position ] @ neighbor_path)))
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        let maze_array = txt_to_array input_file in
        let height = array_height maze_array in
        let length = array_length maze_array in
        let start_pos = find_start_pos maze_array in
        let visited = Hash_set.create (module String) in
        solve ~maze_array ~position:start_pos ~length ~height ~visited
        |> List.iter ~f:(fun x -> print_endline (position_to_string x))]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
