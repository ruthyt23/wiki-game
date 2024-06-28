open! Core

module Article = struct
  module T = struct
    type t =
      { url : string
      ; title : string
      }
    [@@deriving compare, sexp, hash]
  end

  include Comparable.Make (T)
  include T

  let create title url = { title; url }
  let title t = t.title
end

module G = Graph.Imperative.Graph.Concrete (String)

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

let get_linked_articles contents : string list =
  let open Soup in
  parse contents
  $$ "a[href*='/wiki/']"
  |> to_list
  |> List.filter_map ~f:(fun x ->
    match Wikipedia_namespace.namespace (R.attribute "href" x) with
    | None -> Some (R.attribute "href" x)
    | Some _special -> None)
  |> List.dedup_and_sort ~compare:String.compare
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

(* [visualize] should explore all linked articles up to a distance of
   [max_depth] away from the given [origin] article, and output the result as
   a DOT file. It should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory. *)

let no_parens text =
  String.split_on_chars text ~on:[ ' ' ] (* String.substr_replace_all *)
  |> String.concat ~sep:""
  |> String.split_on_chars ~on:[ '(' ]
  |> String.concat ~sep:"_"
  |> String.split_on_chars ~on:[ ')' ]
  |> String.concat
;;

let article_tuples link_prefix article1 article2 =
  ( Article.create
      (String.chop_prefix_exn article1 ~prefix:"/wiki/")
      (link_prefix ^ article1)
  , Article.create
      (String.chop_prefix_exn article2 ~prefix:"/wiki/")
      (link_prefix ^ article2) )
;;

let rec wiki_explore ~max_depth ~origin ~how_to_fetch
  : (Article.t * Article.t) list
  =
  if max_depth = 0
  then []
  else (
    let contents =
      get_linked_articles
        (File_fetcher.fetch_exn how_to_fetch ~resource:origin)
    in
    let to_visit = Hash_set.create (module String) in
    let link_prefix =
      match how_to_fetch with
      | Local root ->
        ignore root;
        ""
      | Remote -> "https://en.wikipedia.org"
    in
    let current_map =
      List.map contents ~f:(fun x ->
        Hash_set.add to_visit x;
        article_tuples link_prefix origin x)
    in
    current_map
    @ List.concat_map (Hash_set.to_list to_visit) ~f:(fun x ->
      wiki_explore ~max_depth:(max_depth - 1) ~origin:x ~how_to_fetch))
;;

let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  let wiki_network =
    wiki_explore ~max_depth ~origin:("/" ^ origin) ~how_to_fetch
  in
  let graph = G.create () in
  List.iter wiki_network ~f:(fun (article1, article2) ->
    G.add_edge
      graph
      (no_parens (Article.title article1))
      (no_parens (Article.title article2)));
  Dot.output_graph
    (Out_channel.create (File_path.to_string output_file))
    graph
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* BFS implementation - doesn't always work let find_path ~origin
   ~destination ~how_to_fetch () = let visited = Hash_set.create (module
   String) in let path = Stack.create () in let to_visit = Queue.create () in
   Queue.enqueue to_visit (origin, []); let found = ref false in while not
   !found do match Queue.dequeue to_visit with | None -> () | Some (page,
   history) -> if String.equal destination page then ( List.iter history
   ~f:(fun x -> Stack.push path x); Stack.push path page; ignore visited;
   ignore how_to_fetch; found.contents <- true) else ( let origin_link =
   match how_to_fetch with | File_fetcher.How_to_fetch.Local root -> ignore
   root; origin | Remote -> "https://en.wikipedia.org" ^ origin in let
   neighbors = get_linked_articles (File_fetcher.fetch_exn how_to_fetch
   ~resource:origin_link) in List.iter neighbors ~f:print_endline;
   List.filter neighbors ~f:(fun x -> not (String.contains x '?' ||
   String.contains x ':')) |> List.iter ~f:(fun neighbor -> match
   Hash_set.find visited ~f:(fun y -> String.equal neighbor y) with | Some _
   -> () | _ -> Queue.enqueue to_visit (neighbor, history @ [ page ]);
   Hash_set.add visited neighbor)) done; Stack.to_list path ;; *)

(* [find_path] should attempt to find a path between the origin article and
   the destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the
   graph. *)

let rec find_path
  ?(max_depth = 3)
  ~origin
  ~destination
  ~visited
  ~how_to_fetch
  ()
  =
  Hash_set.add visited origin;
  if max_depth = 0
  then []
  else if String.equal origin destination
  then [ destination ]
  else (
    let origin_link =
      match how_to_fetch with
      | File_fetcher.How_to_fetch.Local root ->
        ignore root;
        origin
      | Remote -> "https://en.wikipedia.org" ^ origin
    in
    let neighbors =
      get_linked_articles
        (File_fetcher.fetch_exn how_to_fetch ~resource:origin_link)
    in
    List.filter neighbors ~f:(fun x ->
      not (String.contains x '?' || String.contains x ':'))
    |> List.concat_map ~f:(fun neighbor ->
      match Hash_set.find visited ~f:(fun y -> String.equal neighbor y) with
      | Some _ -> []
      | _ ->
        let neighbor_path =
          find_path
            ~max_depth:(max_depth - 1)
            ~origin:neighbor
            ~destination
            ~visited
            ~how_to_fetch
            ()
        in
        (match neighbor_path with
         | [] -> []
         | _ -> [ origin ] @ neighbor_path)))
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Play wiki game by finding a link between the origin and destination \
       pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination =
        flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        let visited = Hash_set.create (module String) in
        let origin_link, destination_link =
          match how_to_fetch with
          | Local root ->
            ignore root;
            origin, destination
          | Remote ->
            ( String.chop_prefix_exn origin ~prefix:"https://en.wikipedia.org"
            , String.chop_prefix_exn
                destination
                ~prefix:"https://en.wikipedia.org" )
        in
        let path =
          find_path
            ~max_depth
            ~origin:origin_link
            ~destination:destination_link
            ~visited
            ~how_to_fetch
            ()
        in
        (* bfs: ignore max_depth; let path = find_path ~origin:origin_link
           ~destination:destination_link ~how_to_fetch () in *)
        if List.is_empty path
        then print_endline "No path found!"
        else List.iter path ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
