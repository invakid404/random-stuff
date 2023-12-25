include Lib.Util;

Py.initialize();

let networkx = Py.import("networkx");

let builtins = Py.Eval.get_builtins();

let digraph = {
  let digraph = Py.Module.get_function(networkx, "DiGraph");

  edges => digraph([|edges|]);
};

let to_list = {
  let list = Py.Dict.find_string(builtins, "list") |> Py.Callable.to_function;

  obj => list([|obj|]);
};

let to_iterator = obj => Py.Module.get_function(obj, "__iter__", [||]);

let each_node = (fn, graph) =>
  to_list(graph) |> to_iterator |> Py.Iter.iter(fn);

let edges = {
  let edges = Py.Module.get_function_with_keywords(networkx, "edges");

  (graph, node) => edges([|graph|], [("nbunch", node)]);
};

let edge_weight = (graph, edge) => {
  Py.Module.get_function(
    graph,
    "get_edge_data",
    [|Py.Tuple.get(edge, 0), Py.Tuple.get(edge, 1)|],
  )
  |> Py.Dict.find_string(_, "weight");
};

let add_edge = (graph, src, dst, weight) => {
  let _ =
    Py.Module.get_function_with_keywords(
      graph,
      "add_edge",
      [|src, dst|],
      [("weight", weight)],
    );
  ();
};

let remove_node = (graph, node) => {
  let _ = Py.Module.get_function(graph, "remove_node", [|node|]);
  ();
};

let all_simple_paths = {
  let all_simple_paths = Py.Module.get_function(networkx, "all_simple_paths");

  (graph, src, dst) => all_simple_paths([|graph, src, dst|]);
};

let path_weight = {
  let path_weight = Py.Module.get_function(networkx, "path_weight");

  (graph, path) =>
    path_weight([|graph, path, "weight" |> Py.String.of_string|]);
};

let list_length = {
  let len = Py.Dict.find_string(builtins, "len") |> Py.Callable.to_function;

  value => len([|value|]);
};

let longest_path = (graph, src, dst) => {
  let result = ref(0);

  let all_paths = all_simple_paths(graph, src, dst);
  Py.Iter.iter(
    path => {
      let curr_weight = path_weight(graph, path) |> Py.Int.to_int;
      if (result^ < curr_weight) {
        result := curr_weight;
      };
    },
    all_paths,
  );

  result^;
};

let add = Py.Number.add;

let print = {
  let print =
    Py.Dict.find_string(builtins, "print") |> Py.Callable.to_function;

  args => {
    let _ = print(args);
    ();
  };
};

type direction =
  | Left
  | Right
  | Up
  | Down;

let delta_of_direction =
  fun
  | Left => (0, (-1))
  | Right => (0, 1)
  | Up => ((-1), 0)
  | Down => (1, 0);

type node =
  | Path
  | Forest
  | Slope(direction);

let node_of_char =
  fun
  | '.' => Path
  | '#' => Forest
  | '<' => Slope(Left)
  | '>' => Slope(Right)
  | '^' => Slope(Up)
  | 'v' => Slope(Down)
  | _ => assert(false);

let in_bounds = (grid, (i, j)) =>
  0 <= i && i < Array.length(grid) && 0 <= j && j < Array.length(grid[0]);

let neighbors = (grid, i, j) => {
  let deltas =
    (
      switch (grid[i][j]) {
      | Path => [Left, Right, Up, Down]
      | Slope(direction) => [direction]
      | Forest => []
      }
    )
    |> List.to_seq
    |> Seq.map(delta_of_direction);

  deltas
  |> Seq.map(((di, dj)) => (i + di, j + dj))
  |> Seq.filter(in_bounds(grid))
  |> Seq.filter(((i, j)) => grid[i][j] != Forest);
};

let parse_grid =
  List.map(String.to_seq >> Seq.map(node_of_char) >> Array.of_seq)
  >> Array.of_list;

let condense_graph = graph => {
  let is_done = ref(false);
  while (! is_done^) {
    is_done := true;

    each_node(
      node => {
        let curr_edges = edges(graph, node) |> to_list;
        let edges_amt = list_length(curr_edges) |> Py.Int.to_int;

        if (edges_amt == 2) {
          is_done := false;

          let edge_a = Py.List.get_item(curr_edges, 0);
          let edge_b = Py.List.get_item(curr_edges, 1);

          let node_a = Py.Tuple.get(edge_a, 1);
          let node_b = Py.Tuple.get(edge_b, 1);

          let weight_a = edge_weight(graph, edge_a);
          let weight_b = edge_weight(graph, edge_b);

          remove_node(graph, node);

          add_edge(graph, node_a, node_b, add(weight_a, weight_b));
          add_edge(graph, node_b, node_a, add(weight_a, weight_b));

          ();
        };
      },
      graph,
    );
  };

  ();
};

let parse_input = lines => {
  let grid = parse_grid(lines);

  let n = Array.length(grid);
  let m = Array.length(grid[0]);

  let edges =
    range(0, n - 1)
    |> Seq.concat_map(i =>
         range(0, m - 1)
         |> Seq.concat_map(j =>
              neighbors(grid, i, j)
              |> Seq.map(((ni, nj)) => ((i, j), (ni, nj)))
            )
       );

  let find_first = row => {
    range(0, m - 1)
    |> Seq.find(col => grid[row][col] != Forest)
    |> Option.map(col => (row, col))
    |> Option.get;
  };

  let src = find_first(0);
  let dst = find_first(n - 1);

  let int_tuple = ((x, y)) =>
    Py.Tuple.of_pair((Py.Int.of_int(x), Py.Int.of_int(y)));

  let edges =
    edges
    |> Seq.map(((src, dst)) =>
         Py.Tuple.of_tuple3((
           int_tuple(src),
           int_tuple(dst),
           Py.Dict.of_bindings_string([("weight", Py.Int.of_int(1))]),
         ))
       )
    |> Py.List.of_seq;

  let graph = digraph(edges);

  (graph, int_tuple(src), int_tuple(dst));
};

let solve = longest_path;

let part1 = lines => {
  let (graph, src, dst) = parse_input(lines);

  solve(graph, src, dst);
};

let slope_pattern = "[<>^v]" |> Str.regexp;

let part2 = lines => {
  let (graph, src, dst) =
    lines |> List.map(Str.global_replace(slope_pattern, ".")) |> parse_input;
  condense_graph(graph);

  solve(graph, src, dst);
};

let () = {
  let lines = read_lines("inputs/day23.txt") |> List.of_seq;

  Printf.printf("%d, %d\n", part1(lines), part2(lines));
};
