include Lib.Util;

type vector = (int, int, int);

let x = ((x, _, _)) => x;
let y = ((_, y, _)) => y;
let z = ((_, _, z)) => z;

let add = ((lx, ly, lz), (rx, ry, rz)) => (lx + rx, ly + ry, lz + rz);

type brick = {
  low: vector,
  high: vector,
  id: int,
};

let points = ({low, high, _}) =>
  range(low |> x, high |> x)
  |> Seq.concat_map(x =>
       range(low |> y, high |> y)
       |> Seq.concat_map(y =>
            range(low |> z, high |> z) |> Seq.map(z => (x, y, z))
          )
     );

let shift = (brick, direction) => {
  ...brick,
  low: add(brick.low, direction),
  high: add(brick.high, direction),
};

let parse_vector = vec => {
  let nums = String.split_on_char(',', vec) |> List.map(int_of_string);
  switch (nums) {
  | [x, y, z] => (x, y, z)
  | _ => assert(false)
  };
};

let parse_brick = (id, line) =>
  switch (String.split_on_char('~', line)) {
  | [left, right] =>
    let left = parse_vector(left);
    let right = parse_vector(right);

    {
      low: (
        min(left |> x, right |> x),
        min(left |> y, right |> y),
        min(left |> z, right |> z),
      ),
      high: (
        max(left |> x, right |> x),
        max(left |> y, right |> y),
        max(left |> z, right |> z),
      ),
      id,
    };
  | _ => assert(false)
  };

let parse_input = Seq.mapi(parse_brick) >> List.of_seq;

let gravity: vector = (0, 0, (-1));

let support_graph = bricks => {
  module Heap =
    Containers.Heap.Make({
      type t = brick;
      let leq = ({low: left, _}, {low: right, _}) => z(left) <= z(right);
    });

  let queue = Heap.add_list(Heap.empty, bricks) |> ref;

  let n = List.length(bricks);

  let graph = Hashtbl.create(n);
  let inverse_graph = Hashtbl.create(n);

  let add_edge = (graph, src, dst) => {
    let edges = Hashtbl.find_opt(graph, src) |> Option.value(~default=[]);
    let exists = List.find_opt(node => dst == node, edges) |> Option.is_some;

    if (!exists) {
      Hashtbl.replace(graph, src, [dst, ...edges]);
    };
  };

  let fixed_points = Hashtbl.create(0);

  while (!Heap.is_empty(queue^)) {
    let (next_queue, brick) = Heap.take_exn(queue^);
    queue := next_queue;

    let prev = ref(brick);
    let below = ref(brick);

    let is_done = ref(false);
    while (! is_done^) {
      let points = points(below^);
      is_done :=
        Seq.find(
          point => Hashtbl.mem(fixed_points, point) || point |> z <= 0,
          points,
        )
        |> Option.is_some;

      if (! is_done^) {
        prev := below^;
        below := shift(below^, gravity);
      };
    };

    points(below^)
    |> Seq.iter(point => {
         let parent = Hashtbl.find_opt(fixed_points, point);
         switch (parent) {
         | Some(parent) =>
           add_edge(graph, brick.id, parent);
           add_edge(inverse_graph, parent, brick.id);
         | None => ()
         };
       });

    points(prev^)
    |> Seq.iter(point => {Hashtbl.add(fixed_points, point, brick.id)});
  };

  (graph, inverse_graph);
};

let part1 = (bricks, (graph, inverse_graph)) =>
  List.filter_map(
    brick => {
      let incoming =
        Hashtbl.find_opt(inverse_graph, brick.id)
        |> Option.value(~default=[]);

      let can_be_removed =
        List.for_all(
          supported => {
            let outgoing =
              Hashtbl.find_opt(graph, supported) |> Option.value(~default=[]);

            List.length(outgoing) > 1;
          },
          incoming,
        );

      can_be_removed ? Some(1) : None;
    },
    bricks,
  )
  |> List.fold_left((+), 0);

let part2 = (bricks, (graph, inverse_graph)) => {
  List.map(
    brick => {
      let queue = Queue.create();
      Queue.add(brick.id, queue);

      let falls = Hashtbl.create(List.length(bricks));
      Hashtbl.add(falls, brick.id, ());

      while (!Queue.is_empty(queue)) {
        let id = Queue.take(queue);

        let incoming =
          Hashtbl.find_opt(inverse_graph, id) |> Option.value(~default=[]);
        List.iter(
          supported => {
            let outgoing =
              Hashtbl.find_opt(graph, supported) |> Option.value(~default=[]);
            let will_fall = List.for_all(Hashtbl.mem(falls), outgoing);

            if (will_fall) {
              Queue.add(supported, queue);
              Hashtbl.replace(falls, supported, ());
            };
          },
          incoming,
        );
      };

      Hashtbl.length(falls) - 1;
    },
    bricks,
  )
  |> List.fold_left((+), 0);
};

let () = {
  let input = read_lines("inputs/day22.txt") |> parse_input;
  let graphs = support_graph(input);

  Printf.printf("%d, %d\n", part1(input, graphs), part2(input, graphs));
};
