open! Core
module City = String

module Interstate = struct
  module Connection = struct
    module T = struct
      type t = City.t * City.t [@@deriving compare, sexp]
    end

    include Comparable.Make (T)
  end

  type t = Connection.Set.t [@@deriving sexp_of]

  let no_periods city =
    String.split_on_chars city ~on:[ ' ' ]
    |> String.concat ~sep:"_"
    |> String.split_on_chars ~on:[ '.' ]
    |> String.concat
  ;;

  let rec pairs_from_interstate city_list : (string * string) list =
    match city_list with
    | [] -> []
    | [ city ] ->
      ignore city;
      []
    | [ city1; city2 ] -> [ city1, city2 ]
    | city1 :: cities ->
      List.map cities ~f:(fun x -> city1, x) @ pairs_from_interstate cities
  ;;

  let connections_from_highway (interstate : string) =
    match String.split ~on:',' interstate with
    | _ :: tail -> List.map ~f:no_periods tail |> pairs_from_interstate
    | _ -> []
  ;;

  let of_file input_file =
    let interstate_system_lines =
      In_channel.read_lines (File_path.to_string input_file)
    in
    let interstate_system =
      List.concat_map interstate_system_lines ~f:(fun x ->
        connections_from_highway x)
    in
    Connection.Set.of_list interstate_system
  ;;
end

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        let interstate_system = Interstate.of_file input_file in
        printf !"%{sexp: Interstate.t}\n" interstate_system]
;;

module G = Graph.Imperative.Graph.Concrete (City)

module Dot = Graph.Graphviz.Dot (struct
    include G

    let edge_attributes _ = [ `Dir `None ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        let interstate_system = Interstate.of_file input_file in
        let graph = G.create () in
        Set.iter interstate_system ~f:(fun (city1, city2) ->
          G.add_edge graph city1 city2);
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
