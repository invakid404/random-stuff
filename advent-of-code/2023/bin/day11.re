include Lib.Util;

type input = {
  image: array(array(char)),
  galaxies: array((int, int)),
  empty_rows: array(bool),
  empty_cols: array(bool),
};

let collect_empty = (~flip=false, image) => {
  let n = Array.length(image);
  let m = Array.length(image[0]);

  let set = Array.make(flip ? m : n, false);

  for (i in 0 to flip ? m - 1 : n - 1) {
    let j = ref(0);
    let is_empty = ref(true);

    while (is_empty^ && j^ < (flip ? n : m)) {
      let value = flip ? image[j^][i] : image[i][j^];

      if (value != '.') {
        is_empty := false;
      };

      j := j^ + 1;
    };

    if (is_empty^) {
      set[i] = true;
    };
  };

  set;
};

let collect_galaxies = image => {
  let galaxies = ref([]);

  let n = Array.length(image);
  let m = Array.length(image[0]);

  for (i in 0 to n - 1) {
    for (j in 0 to m - 1) {
      if (image[i][j] == '#') {
        galaxies := [(i, j), ...galaxies^];
      };
    };
  };

  Array.of_list(galaxies^);
};

let parse_input = input => {
  let image = input |> Seq.map(String.to_seq >> Array.of_seq) |> Array.of_seq;

  let empty_rows = collect_empty(image);
  let empty_cols = collect_empty(image, ~flip=true);

  let galaxies = collect_galaxies(image);

  {image, galaxies, empty_rows, empty_cols};
};

let distance = ((x1, y1), (x2, y2)) => abs(x2 - x1) + abs(y2 - y1);

let count_empty = (empty, x, y) => {
  let left = min(x, y);
  let right = max(x, y);

  let count = ref(0);
  for (i in left + 1 to right - 1) {
    if (empty[i]) {
      count := count^ + 1;
    };
  };

  count^;
};

let calculate_distances = input => {
  let n = Array.length(input.galaxies);

  let distances = ref([]);

  for (i in 0 to n - 1) {
    let galaxy = input.galaxies[i];
    let (galaxy_x, galaxy_y) = galaxy;

    for (j in i + 1 to n - 1) {
      let other = input.galaxies[j];
      let (other_x, other_y) = other;

      let dist = distance(galaxy, other);
      let empty_rows = count_empty(input.empty_rows, galaxy_x, other_x);
      let empty_cols = count_empty(input.empty_cols, galaxy_y, other_y);

      distances := [(dist, empty_rows, empty_cols), ...distances^];
    };
  };

  distances^;
};

let sum_distances = (multiplier, distances) =>
  distances
  |> List.fold_left(
       (acc, (dist, empty_rows, empty_cols)) =>
         acc + dist + (empty_rows + empty_cols) * (multiplier - 1),
       0,
     );

let part1 = sum_distances(2);

let part2 = sum_distances(1_000_000);

let () = {
  let input = read_lines("inputs/day11.txt") |> parse_input;
  let distances = calculate_distances(input);

  Printf.printf("%d, %d\n", part1(distances), part2(distances));
};
