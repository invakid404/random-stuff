include Lib.Util;

Py.initialize();

let networkx = Py.import("networkx");
let builtins = Py.Eval.get_builtins();

let make_graph = edges =>
  Py.Module.get_function(networkx, "Graph", [|edges|]);

let min_cut = graph =>
  Py.Module.get_function(networkx, "minimum_edge_cut", [|graph|]);

let remove_edges_from = (graph, edges) => {
  let _ = Py.Module.get_function(graph, "remove_edges_from", [|edges|]);
  ();
};

let to_list = value =>
  Py.Dict.find_string(builtins, "list")
  |> Py.Callable.to_function(_, [|value|]);

let connected_components = graph =>
  Py.Module.get_function(networkx, "connected_components", [|graph|])
  |> to_list;

let len = list =>
  Py.Dict.find_string(builtins, "len")
  |> Py.Callable.to_function(_, [|list|])
  |> Py.Int.to_int;

let parse_input =
  Seq.map(
    Str.global_replace(":" |> Str.regexp_string, "")
    >> String.split_on_char(' '),
  )
  >> List.of_seq
  >> List.concat_map(nodes => {
       switch (nodes) {
       | [src, ...dests] => List.map(dest => (src, dest), dests)
       | _ => assert(false)
       }
     })
  >> List.map(((src, dest)) =>
       Py.Tuple.of_pair((
         src |> Py.String.of_string,
         dest |> Py.String.of_string,
       ))
     )
  >> Py.List.of_list
  >> make_graph;

let () = {
  let graph = read_lines("inputs/day25.txt") |> parse_input;

  let edges = min_cut(graph);
  remove_edges_from(graph, edges);

  let components =
    connected_components(graph) |> Py.List.to_list |> List.map(len);

  let result =
    switch (components) {
    | [a, b] => a * b
    | _ => assert(false)
    };

  Printf.printf("%d\n", result);
};
