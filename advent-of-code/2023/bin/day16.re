include Lib.Util;

type rotation =
  | A
  | B;

type direction =
  | Vertical
  | Horizontal;

type grid_node =
  | Empty
  | Mirror(rotation)
  | Splitter(direction);

let grid_node_of_char =
  fun
  | '.' => Empty
  | '/' => Mirror(A)
  | '\\' => Mirror(B)
  | '|' => Splitter(Vertical)
  | '-' => Splitter(Horizontal)
  | _ => assert(false);

let parse_row = String.to_seq >> Seq.map(grid_node_of_char) >> Array.of_seq;

let parse_input = Seq.map(parse_row) >> Array.of_seq;

let trace = ((x, y), (dx, dy), input) => {
  let n = Array.length(input);
  let m = Array.length(input[0]);

  let in_bounds = (x, y) => 0 <= x && x < n && 0 <= y && y < m;

  let visited = Hashtbl.create(n * m);
  let energized = Array.make_matrix(n, m, false);

  let queue = Queue.create();
  Queue.add((x, y, dx, dy), queue);

  let get = (x, y) => in_bounds(x, y) ? Some(input[x][y]) : None;

  let add = (x, y, dx, dy) =>
    if (in_bounds(x, y)) {
      Queue.add((x, y, dx, dy), queue);
    };

  let total = ref(0);

  while (!Queue.is_empty(queue)) {
    let node = Queue.take(queue);
    if (!Hashtbl.mem(visited, node)) {
      let (x, y, dx, dy) = node;
      if (in_bounds(x, y)) {
        if (!energized[x][y]) {
          total := total^ + 1;
        };

        energized[x][y] = true;
      };
      Hashtbl.add(visited, node, ());

      let (next_x, next_y) = (x + dx, y + dy);
      switch (get(next_x, next_y)) {
      | Some(Empty) => add(next_x, next_y, dx, dy)
      | Some(Mirror(rotation)) =>
        switch (rotation) {
        | A => add(next_x, next_y, - dy, - dx)
        | B => add(next_x, next_y, dy, dx)
        }
      | Some(Splitter(direction)) =>
        switch (direction) {
        | Vertical when dx == 0 =>
          add(next_x, next_y, -1, 0);
          add(next_x, next_y, 1, 0);
        | Horizontal when dy == 0 =>
          add(next_x, next_y, 0, -1);
          add(next_x, next_y, 0, 1);
        | _ => add(next_x, next_y, dx, dy)
        }
      | None => ()
      };
    };
  };

  total^;
};

let part1 = trace((0, (-1)), (0, 1));

let part2 = input => {
  let n = Array.length(input);
  let m = Array.length(input[0]);

  let top =
    range(0, m - 1) |> Seq.map(y => trace(((-1), y), (1, 0), input));
  let bottom =
    range(0, m - 1) |> Seq.map(y => trace((n, y), ((-1), 0), input));
  let left =
    range(0, n - 1) |> Seq.map(x => trace((x, (-1)), (0, 1), input));
  let right =
    range(0, n - 1) |> Seq.map(x => trace((x, m), (0, (-1)), input));

  top
  |> Seq.append(bottom)
  |> Seq.append(left)
  |> Seq.append(right)
  |> Seq.fold_left(max, 0);
};

let () = {
  let input = read_lines("inputs/day16.txt") |> parse_input;

  Printf.printf("%d, %d\n", part1(input), part2(input));
};
