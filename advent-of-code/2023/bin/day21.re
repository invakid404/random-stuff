include Lib.Util;

type cell =
  | Empty
  | Rock
  | Start;

let cell_of_char =
  fun
  | '.' => Empty
  | '#' => Rock
  | 'S' => Start
  | _ => assert(false);

type input = {
  grid: array(array(cell)),
  start: (int, int),
};

let parse_grid =
  Seq.map(String.to_seq >> Seq.map(cell_of_char) >> Array.of_seq)
  >> Array.of_seq;

let parse_input = lines => {
  let grid = parse_grid(lines);

  let n = Array.length(grid);
  let m = Array.length(grid);

  let start =
    range(0, n - 1)
    |> Seq.find_map(i =>
         range(0, m - 1)
         |> Seq.find(j => grid[i][j] == Start)
         |> Option.map(j => (i, j))
       )
    |> Option.get;

  {grid, start};
};

let wrap = (l, r) => (l mod r + r) mod r;

let neighbors = (grid, i, j) =>
  [((-1), 0), (1, 0), (0, (-1)), (0, 1)]
  |> List.to_seq
  |> Seq.map(((di, dj)) => (i + di, j + dj))
  |> Seq.filter(((i, j)) => {
       let (mi, mj) = (
         wrap(i, Array.length(grid)),
         wrap(j, Array.length(grid[0])),
       );

       grid[mi][mj] != Rock;
     });

let solve = input => {
  let n = Array.length(input.grid);
  let m = Array.length(input.grid[0]);

  let neighbors = neighbors(input.grid);

  let queue = Queue.create();
  Queue.add(input.start, queue);

  let visited = Hashtbl.create(10 * n * m);

  let part1 = ref(None);
  let part2 = ref([]);

  for (depth in 0 to 3 * 131 - 1) {
    Hashtbl.clear(visited);
    let current = Queue.length(queue);

    if (depth == 64) {
      part1 := Some(current);
    };

    if (depth mod 131 == 65) {
      part2 := [current, ...part2^];
    };

    for (_ in 1 to current) {
      let (i, j) = Queue.take(queue);

      let next = neighbors(i, j);
      Seq.iter(
        pos =>
          if (!Hashtbl.mem(visited, pos)) {
            Queue.add(pos, queue);
            Hashtbl.add(visited, pos, ());
          },
        next,
      );
    };
  };

  switch (part2^) {
  | [c, b, a] =>
    let part1 = part1^ |> Option.get;

    let n = 26501365 / 131;
    let part2 = a + n * (b - a + (n - 1) * (c - 2 * b + a) / 2);

    (part1, part2);
  | _ => assert(false)
  };
};

let () = {
  let input = read_lines("inputs/day21.txt") |> parse_input;

  let (part1, part2) = solve(input);
  Printf.printf("%d, %d\n", part1, part2);
};
