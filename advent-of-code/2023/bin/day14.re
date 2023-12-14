include Lib.Util;

type grid_node =
  | Empty
  | Rounded
  | Cube;

let grid_node_of_char =
  fun
  | '.' => Empty
  | 'O' => Rounded
  | '#' => Cube
  | _ => assert(false);

let int_of_grid_node =
  fun
  | Empty => 1
  | Rounded => 2
  | Cube => 3;

let parse_input =
  Seq.map(String.to_seq >> Seq.map(grid_node_of_char) >> Array.of_seq)
  >> Array.of_seq;

let rotate = grid => {
  let n = Array.length(grid);
  let m = Array.length(grid[0]);

  let new_grid = Array.make_matrix(m, n, Empty);

  for (i in 0 to n - 1) {
    for (j in 0 to m - 1) {
      new_grid[j][n - i - 1] = grid[i][j];
    };
  };

  new_grid;
};

let roll = grid => {
  let n = Array.length(grid);
  let m = Array.length(grid[0]);

  for (j in 0 to m - 1) {
    let changed = ref(true);
    while (changed^) {
      changed := false;

      for (i in n - 1 downto 1) {
        if (grid[i][j] == Rounded && grid[i - 1][j] == Empty) {
          grid[i][j] = Empty;
          grid[i - 1][j] = Rounded;

          changed := true;
        };
      };
    };
  };

  grid;
};

let score = grid => {
  let total = ref(0);

  let n = Array.length(grid);
  let m = Array.length(grid[0]);

  for (i in 0 to n - 1) {
    for (j in 0 to m - 1) {
      if (grid[i][j] == Rounded) {
        total := total^ + n - i;
      };
    };
  };

  total^;
};

let hash = grid => {
  let res = ref(0);

  let n = Array.length(grid);
  let m = Array.length(grid[0]);

  for (i in 0 to n - 1) {
    for (j in 0 to m - 1) {
      let c = int_of_grid_node(grid[i][j]);
      let p = i * n + j;

      res := res^ * 31 + c + p;
    };
  };

  res^;
};

let target = 1_000_000_000;

let solve = grid => {
  let curr_grid = ref(grid);
  let iters = ref(0);

  let part1 = ref(0);

  let seen = Hashtbl.create(150);

  while (iters^ < target) {
    iters := iters^ + 1;

    for (dir in 1 to 4) {
      curr_grid := roll(curr_grid^);
      if (iters^ == 1 && dir == 1) {
        part1 := score(curr_grid^);
      };

      curr_grid := rotate(curr_grid^);
    };

    let h = hash(curr_grid^);
    switch (Hashtbl.find_opt(seen, h)) {
    | Some(stored_iters) =>
      let cycle_length = iters^ - stored_iters;
      let amount = (target - iters^) / cycle_length;

      iters := iters^ + amount * cycle_length;
    | None => ()
    };

    Hashtbl.replace(seen, h, iters^);
  };

  let part2 = score(curr_grid^);

  (part1^, part2);
};

let () = {
  let input = read_lines("inputs/day14.txt") |> parse_input;
  let (part1, part2) = solve(input);

  Printf.printf("%d, %d\n", part1, part2);
};
