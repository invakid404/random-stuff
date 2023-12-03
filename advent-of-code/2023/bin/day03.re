include Lib.Util;

let check_neighbors = (grid, i, j, len) => {
  let clamp_x = clamp(0, Array.length(grid) - 1);
  let clamp_y = clamp(0, Array.length(grid[0]) - 1);

  let x1 = clamp_x(i - 1);
  let y1 = clamp_y(j - len - 1);
  let x2 = clamp_x(i + 1);
  let y2 = clamp_y(j);

  range(x1, x2)
  |> Seq.flat_map(x =>
       range(y1, y2)
       |> Seq.filter(y => !is_digit(grid[x][y]) && grid[x][y] != '.')
       |> Seq.map(y => (x, y))
     );
};

let each_number = (f, grid) => {
  let curr = ref(0);
  let len = ref(0);

  let reset = (i, j) =>
    if (len^ != 0) {
      let neighbors = check_neighbors(grid, i, j, len^);
      if (!Seq.is_empty(neighbors)) {
        f(curr^, neighbors);
      };
      curr := 0;
      len := 0;
    };

  for (i in 0 to Array.length(grid) - 1) {
    for (j in 0 to Array.length(grid[i]) - 1) {
      let c = grid[i][j];
      if (is_digit(c)) {
        curr := curr^ * 10 + digit_to_int(c);
        len := len^ + 1;
      } else {
        reset(i, j);
      };
    };

    reset(i, Array.length(grid[i]));
  };
};

let part1 = grid => {
  let acc = ref(0);

  each_number((curr, _neighbors) => acc := acc^ + curr, grid);

  acc^;
};

let part2 = grid => {
  let seen: Hashtbl.t((int, int), list(int)) =
    Hashtbl.create(3 * Array.length(grid));

  each_number(
    (curr, neighbors) =>
      neighbors
      |> Seq.filter(((i, j)) => grid[i][j] == '*')
      |> S.for_each(key => {
           let existing =
             Hashtbl.find_opt(seen, key) |> Option.value(~default=[]);
           Hashtbl.replace(seen, key, [curr, ...existing]);
         }),
    grid,
  );

  Hashtbl.to_seq_values(seen)
  |> Seq.filter_map(v =>
       switch (v) {
       | [a, b] => Some(a * b)
       | _ => None
       }
     )
  |> Seq.fold_left((+), 0);
};

let () = {
  let grid =
    read_lines("inputs/day03.txt")
    |> Seq.map(String.to_seq)
    |> Seq.map(Array.of_seq)
    |> Array.of_seq;

  Printf.printf("%d, %d\n", part1(grid), part2(grid));
};
