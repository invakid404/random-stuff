include Lib.Util;

type maze = {
  grid: array(array(char)),
  start: (int, int),
};

type direction =
  | North
  | South
  | East
  | West;

let next_coords = (dir, (x, y)) =>
  switch (dir) {
  | North => (x - 1, y)
  | South => (x + 1, y)
  | East => (x, y + 1)
  | West => (x, y - 1)
  };

let opposite =
  fun
  | North => South
  | South => North
  | East => West
  | West => East;

let is_connected = (grid, (x, y), dir) =>
  switch (grid[x][y], dir) {
  | ('|', North | South)
  | ('-', East | West)
  | ('L', North | East)
  | ('J', North | West)
  | ('7', South | West)
  | ('F', South | East) => true
  | _ => false
  };

let in_bounds = (grid, (x, y)) =>
  0 <= x && x < Array.length(grid) && 0 <= y && y < Array.length(grid[0]);

let all_directions = [North, South, East, West];

let neighbors = (grid, pos) =>
  all_directions
  |> List.filter_map(dir => {
       let next = next_coords(dir, pos);

       if (in_bounds(grid, next) && is_connected(grid, next, opposite(dir))) {
         Some((next, dir));
       } else {
         None;
       };
     });

let connect = (x, y) =>
  switch (x, y) {
  | (North, South) => '|'
  | (East, West) => '-'
  | (North, East) => 'L'
  | (North, West) => 'J'
  | (South, West) => '7'
  | (South, East) => 'F'
  | _ => assert(false)
  };

let replace_start = (grid, start) => {
  let to_connect = neighbors(grid, start) |> List.map(snd);

  switch (to_connect) {
  | [x, y] => connect(x, y)
  | _ => assert(false)
  };
};

let find_in_grid = (grid, c) => {
  let result = ref(None);

  let i = ref(0);
  let j = ref(0);

  let n = Array.length(grid);
  let m = Array.length(grid[0]);

  while (Option.is_none(result^) && i^ < n) {
    if (j^ == m) {
      i := i^ + 1;
      j := 0;
    };

    if (grid[i^][j^] == c) {
      result := Some((i^, j^));
    };

    j := j^ + 1;
  };

  result^;
};

let parse_input = input => {
  let grid = Seq.map(String.to_seq >> Array.of_seq, input) |> Array.of_seq;
  let start = find_in_grid(grid, 'S') |> Option.get;

  let (x, y) = start;
  grid[x][y] = replace_start(grid, start);

  {grid, start};
};

let restore_path = (start, parent) => {
  let rec restore_path_helper = (pos, acc) => {
    let (x, y) = pos;

    switch (parent[x][y]) {
    | Some(next) when next != start =>
      restore_path_helper(next, [pos, ...acc])
    | _ => List.rev([pos, ...acc])
    };
  };

  restore_path_helper(start, []);
};

let find_loop = ({grid, start}) => {
  let n = Array.length(grid);
  let m = Array.length(grid[0]);

  let parents = Array.make_matrix(n, m, None);

  let rec find_loop_helper = (pos, parent) => {
    let (x, y) = pos;
    parents[x][y] = parent;

    neighbors(grid, pos)
    |> L.for_each(((next, _dir)) => {
         let (nx, ny) = next;
         if (Option.is_none(parents[nx][ny]) && Some(next) != parent) {
           find_loop_helper(next, Some(pos));
         };
       });
  };

  find_loop_helper(start, None);
  restore_path(start, parents);
};

let part1 = loop => {
  List.length(loop) / 2;
};

let part2 = ({grid, _}, loop) => {
  let in_loop = Hashtbl.create(List.length(loop));
  L.for_each(pos => Hashtbl.add(in_loop, pos, ()), loop);

  let total = ref(0);
  for (x in 0 to Array.length(grid) - 1) {
    let is_in = ref(false);

    for (y in 0 to Array.length(grid[0]) - 1) {
      if (Hashtbl.mem(in_loop, (x, y))) {
        switch (grid[x][y]) {
        | '|'
        | 'L'
        | 'J' => is_in := ! is_in^
        | _ => ()
        };
      } else if (is_in^) {
        total := total^ + 1;
      };
    };
  };

  total^;
};

let () = {
  let input = read_lines("inputs/day10.txt") |> parse_input;
  let loop = find_loop(input);

  Printf.printf("%d, %d\n", part1(loop), part2(input, loop));
};
