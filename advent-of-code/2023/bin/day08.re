include Lib.Util;

type instruction =
  | Left
  | Right;

let instruction_of_char =
  fun
  | 'L' => Left
  | 'R' => Right
  | _ => assert(false);

let next_by_instruction = (instruction, edge) =>
  switch (instruction) {
  | Left => fst(edge)
  | Right => snd(edge)
  };

type graph = Hashtbl.t(string, (string, string));

type network = {
  instructions: array(instruction),
  graph,
};

let parse_instructions = instructions =>
  String.to_seq(instructions) |> Seq.map(instruction_of_char) |> Array.of_seq;

let edge_pattern = {|\([A-Z]+\) = (\([A-Z]+\), \([A-Z]+\))|} |> Str.regexp;

let parse_edge = edge => {
  assert(Str.string_match(edge_pattern, edge, 0));

  let node = Str.matched_group(1, edge);
  let left = Str.matched_group(2, edge);
  let right = Str.matched_group(3, edge);

  (node, (left, right));
};

let parse_graph = edges => {
  let graph = Hashtbl.create(List.length(edges));
  List.to_seq(edges) |> Seq.map(parse_edge) |> Hashtbl.add_seq(graph);

  graph;
};

let parse_input =
  fun
  | [instructions, _, ...edges] => {
      instructions: parse_instructions(instructions),
      graph: parse_graph(edges),
    }
  | _ => assert(false);

let distance = (source, f, {instructions, graph}) => {
  let node = ref(source);
  let steps = ref(0);

  while (!f(node^)) {
    let instruction = instructions[steps^ mod Array.length(instructions)];
    let edges = Hashtbl.find(graph, node^);

    node := next_by_instruction(instruction, edges);
    steps := steps^ + 1;
  };

  steps^;
};

let part1 = distance("AAA", node => node == "ZZZ");

let part2 = input => {
  let start_nodes =
    input.graph
    |> Hashtbl.to_seq_keys
    |> Seq.filter(String.ends_with(~suffix="A"));

  let distances =
    start_nodes
    |> Seq.map(node => distance(node, String.ends_with(~suffix="Z"), input));

  distances |> Seq.fold_left(lcm, 1);
};

let () = {
  let input = read_lines("inputs/day08.txt") |> List.of_seq |> parse_input;

  Printf.printf("%d, %d\n", part1(input), part2(input));
};
