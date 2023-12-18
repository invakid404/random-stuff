include Lib.Util;

type direction =
  | Up
  | Down
  | Left
  | Right;

let direction_of_string =
  fun
  | "U" => Up
  | "D" => Down
  | "L" => Left
  | "R" => Right
  | _ => assert(false);

let string_of_direction =
  fun
  | Up => "U"
  | Down => "D"
  | Left => "L"
  | Right => "R";

let delta =
  fun
  | Down => (0, (-1))
  | Up => (0, 1)
  | Left => ((-1), 0)
  | Right => (1, 0);

type instruction = {
  direction,
  distance: int,
  color: string,
};

let parse_instruction =
  String.split_on_char(' ')
  >> (
    fun
    | [direction, distance, color] => {
        direction: direction_of_string(direction),
        distance: int_of_string(distance),
        color: String.sub(color, 2, String.length(color) - 3),
      }
    | _ => assert(false)
  );

let parse_input = Seq.map(parse_instruction) >> List.of_seq;

let solve = instructions => {
  let x = ref(0);
  let y = ref(0);

  let area_a = ref(0);
  let area_b = ref(0);
  let total_distance = ref(0);

  List.iter(
    ({direction, distance, _}) => {
      let (dx, dy) = delta(direction);
      let (nx, ny) = (x^ + dx * distance, y^ + dy * distance);

      area_a := area_a^ + x^ * ny;
      area_b := area_b^ + y^ * nx;
      total_distance := total_distance^ + distance;

      x := nx;
      y := ny;
    },
    instructions,
  );

  abs(area_a^ - area_b^) / 2 + total_distance^ / 2 + 1;
};

let part1 = solve;

let from_color = instruction => {
  let n = String.length(instruction.color) - 1;
  let distance = "0x" ++ String.sub(instruction.color, 0, n) |> int_of_string;

  let direction =
    switch (instruction.color.[n]) {
    | '0' => Right
    | '1' => Down
    | '2' => Left
    | '3' => Up
    | _ => assert(false)
    };

  {...instruction, distance, direction};
};

let part2 = List.map(from_color) >> part1;

let () = {
  let input = read_lines("inputs/day18.txt") |> parse_input;

  Printf.printf("%d, %d\n", part1(input), part2(input));
};
