include Lib.Util;

type races = list((int, int));

let nums_delimiter = "[ \t]+" |> Str.regexp;

let parse_nums = nums =>
  Str.split(nums_delimiter, nums) |> List.map(int_of_string);

let word_delimiter = ":" |> Str.regexp_string;

let parse_times = times =>
  switch (Str.split(word_delimiter, times)) {
  | ["Time", times] => String.trim(times) |> parse_nums
  | _ => assert(false)
  };

let parse_distances = distances =>
  switch (Str.split(word_delimiter, distances)) {
  | ["Distance", distances] => String.trim(distances) |> parse_nums
  | _ => assert(false)
  };

let parse_input = input =>
  switch (input) {
  | [times, distances] =>
    List.combine(parse_times(times), parse_distances(distances))
  | _ => assert(false)
  };

let calculate = (time, distance) => {
  let time = float_of_int(time);
  let distance = float_of_int(distance);

  let d = Float.sqrt(time *. time -. 4. *. distance);

  let x1 = Float.floor((time +. d) /. 2.);
  let x2 = Float.ceil((time -. d) /. 2.);

  truncate(x1 -. x2) + 1;
};

let part1 = input =>
  input
  |> List.fold_left(
       (acc, (time, record)) => acc * calculate(time, record),
       1,
     );

let part2 = input => {
  let (time, distance) =
    List.fold_left(
      ((total_time, total_distance), (time, distance)) => {
        (int_concat(total_time, time), int_concat(total_distance, distance))
      },
      (0, 0),
      input,
    );

  calculate(time, distance);
};

let () = {
  let input = read_lines("inputs/day06.txt") |> List.of_seq |> parse_input;

  Printf.printf("%d, %d\n", part1(input), part2(input));
};
