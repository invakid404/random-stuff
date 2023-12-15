include Lib.Util;

let boxes_amt = 256;

let hash =
  String.fold_left(
    (acc, c) => (acc + int_of_char(c)) * 17 mod boxes_amt,
    0,
  );

let parse_input = String.trim >> String.split_on_char(',');

let part1 = List.map(hash) >> List.fold_left((+), 0);

type operation =
  | Dash
  | Equals(int);

type instruction = {
  label: string,
  hash: int,
  operation,
};

let parse_equals = input =>
  switch (String.split_on_char('=', input)) {
  | [label, value] => {
      label,
      hash: hash(label),
      operation: Equals(int_of_string(value)),
    }
  | _ => assert(false)
  };

let parse_dash = input => {
  let label = String.sub(input, 0, String.length(input) - 1);

  {label, hash: hash(label), operation: Dash};
};

let parse_instruction = input =>
  String.contains(input, '=') ? parse_equals(input) : parse_dash(input);

let part2 = input => {
  let instructions = List.map(parse_instruction, input);
  let boxes = Array.make(boxes_amt, []);

  L.for_each(
    instruction => {
      let current = boxes[instruction.hash];

      boxes[instruction.hash] = (
        switch (instruction.operation) {
        | Dash =>
          List.filter(
            ((label, _value)) => label != instruction.label,
            current,
          )
        | Equals(n) =>
          L.upsert_first(
            ((label, _value)) => label == instruction.label,
            (instruction.label, n),
            current,
          )
        }
      );
    },
    instructions,
  );

  Array.to_seqi(boxes)
  |> Seq.map(((box, values)) =>
       List.mapi(
         (idx, (_label, value)) => (box + 1) * (idx + 1) * value,
         values,
       )
       |> List.fold_left((+), 0)
     )
  |> Seq.fold_left((+), 0);
};

let () = {
  let input = read_all("inputs/day15.txt") |> parse_input;

  Printf.printf("%d, %d\n", part1(input), part2(input));
};
