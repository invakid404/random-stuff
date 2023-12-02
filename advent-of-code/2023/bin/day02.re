include Lib.Util;

type cube_color =
  | Red
  | Green
  | Blue;

let parse_color = color =>
  switch (color) {
  | "red" => Red
  | "green" => Green
  | "blue" => Blue
  | _ => assert(false)
  };

type game_cubes = Hashtbl.t(cube_color, int);

let cube_delimiter = " " |> Str.regexp_string;

let parse_cube = cube =>
  switch (Str.split(cube_delimiter, cube)) {
  | [amount_str, color_str] => (
      parse_color(color_str),
      int_of_string(amount_str),
    )
  | _ => assert(false)
  };

let cubes_color = (color, cubes) =>
  Hashtbl.find_opt(cubes, color) |> Option.value(~default=0);

let cubes_delimiter = "," |> Str.regexp_string;

let parse_cubes = cubes =>
  Str.split(cubes_delimiter, cubes)
  |> List.map(String.trim)
  |> List.map(parse_cube)
  |> (
    cubes => {
      let map = Hashtbl.create(List.length(cubes));
      Hashtbl.add_seq(map, List.to_seq(cubes));

      map;
    }
  );

type game = {
  id: int,
  rounds: list(game_cubes),
};

let game_id_delimiter = " " |> Str.regexp_string;

let parse_game_id = game_id =>
  switch (Str.split(game_id_delimiter, game_id)) {
  | ["Game", id] => int_of_string(id)
  | _ => assert(false)
  };

let game_delimiter = ":" |> Str.regexp_string;
let rounds_delimiter = ";" |> Str.regexp_string;

let parse_game = game =>
  switch (Str.split(game_delimiter, game)) {
  | [game_id_str, rounds_str] => {
      id: parse_game_id(game_id_str),
      rounds:
        Str.split(rounds_delimiter, rounds_str)
        |> List.map(String.trim)
        |> List.map(parse_cubes),
    }
  | _ => assert(false)
  };

let part1 = games =>
  games
  |> List.filter(game =>
       game.rounds
       |> List.for_all(round =>
            cubes_color(Red, round) <= 12
            && cubes_color(Green, round) <= 13
            && cubes_color(Blue, round) <= 14
          )
     )
  |> List.fold_left((acc, game) => acc + game.id, 0);

let merge = (acc, cubes) => {
  Hashtbl.to_seq(cubes)
  |> S.for_each(((k, v)) =>
       Hashtbl.replace(acc, k, cubes_color(k, acc) |> max(v))
     );

  acc;
};

let part2 = games =>
  games
  |> List.map(game =>
       game.rounds
       |> List.fold_left(merge, Hashtbl.create(3))
       |> Hashtbl.to_seq_values
       |> Seq.fold_left(( * ), 1)
     )
  |> List.fold_left((+), 0);

let () = {
  let games =
    read_lines("inputs/day02.txt") |> Seq.map(parse_game) |> List.of_seq;

  Printf.printf("%d, %d\n", part1(games), part2(games));
};
