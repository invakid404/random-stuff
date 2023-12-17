include Lib.Util;

let parse_input =
  Seq.map(String.to_seq >> Seq.map(digit_to_int) >> Array.of_seq)
  >> Array.of_seq;

type direction =
  | Left
  | Right
  | Up
  | Down;

let delta =
  fun
  | Left => (0, (-1))
  | Right => (0, 1)
  | Up => ((-1), 0)
  | Down => (1, 0);

let rotate =
  fun
  | Left
  | Right => [Down, Up]
  | Up
  | Down => [Right, Left];

let min_heat = (min_moves, max_moves, grid) => {
  let n = Array.length(grid);
  let m = Array.length(grid[0]);

  let in_bounds = (x, y) => 0 <= x && x < n && 0 <= y && y < m;

  module Heap =
    Containers.Heap.Make({
      type t = (int, (int, int), direction);
      let leq = ((left, _, _), (right, _, _)) => left <= right;
    });

  let queue = ref(Heap.empty);
  queue := Heap.add(queue^, (0, (0, 0), Up));

  let seen = Hashtbl.create(4 * n * m);

  let res = ref(None);

  while (Option.is_none(res^) && !Heap.is_empty(queue^)) {
    let (next_queue, node) = Heap.take_exn(queue^);
    queue := next_queue;

    let (heat, (x, y), direction) = node;
    let seen_key = (x, y, direction);

    if (x == n - 1 && y == m - 1) {
      res := Some(heat);
    } else if (!Hashtbl.mem(seen, seen_key)) {
      Hashtbl.add(seen, seen_key, ());

      let turns = rotate(direction);
      List.iter(
        current_direction => {
          let (dx, dy) = delta(current_direction);

          let coords =
            range(1, max_moves)
            |> S.scan_left(((x, y), _) => (x + dx, y + dy), (x, y))
            |> Seq.take_while(((x, y)) => in_bounds(x, y))
            |> Seq.map(((x, y)) => (heat, x, y))
            |> S.scan_left1(((heat, _, _), (_, x, y)) =>
                 (heat + grid[x][y], x, y)
               )
            |> Seq.drop(min_moves - 1)
            |> Seq.map(((heat, x, y)) => (heat, (x, y), current_direction));

          queue := Heap.add_seq(queue^, coords);
        },
        turns,
      );
    };
  };

  res^ |> Option.get;
};

let part1 = min_heat(1, 3);

let part2 = min_heat(4, 10);

let () = {
  let input = read_lines("inputs/day17.txt") |> parse_input;

  Printf.printf("%d, %d\n", part1(input), part2(input));
};
