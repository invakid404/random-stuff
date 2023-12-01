include Lib.Util;

let solve = (pattern, parse, lines) => {
  let extract_digit = line => Str.matched_string(line) |> parse;

  lines
  |> List.map(line => {
       let _ = Str.search_forward(pattern, line, 0);
       let left_digit = extract_digit(line);

       let _ = Str.search_backward(pattern, line, String.length(line) - 1);
       let right_digit = extract_digit(line);

       left_digit * 10 + right_digit;
     })
  |> List.fold_left((+), 0);
};

let part1 = solve(Str.regexp("[0-9]"), int_of_string);

let digits = [
  ("one", "1"),
  ("two", "2"),
  ("three", "3"),
  ("four", "4"),
  ("five", "5"),
  ("six", "6"),
  ("seven", "7"),
  ("eight", "8"),
  ("nine", "9"),
];

let part2_pattern =
  digits
  |> List.map(fst)
  |> List.append(["[0-9]"])
  |> join("\\|")
  |> Str.regexp;

let digit_map: Hashtbl.t(string, int) = Hashtbl.create(List.length(digits));

digits
|> List.map(((src, dst)) => (src, int_of_string(dst)))
|> List.to_seq
|> Hashtbl.add_seq(digit_map);

let part2 =
  solve(part2_pattern, m =>
    String.length(m) > 1 ? Hashtbl.find(digit_map, m) : int_of_string(m)
  );

let () = {
  let lines = read_lines("inputs/day01.txt") |> List.of_seq;

  Printf.printf("%d, %d\n", part1(lines), part2(lines));
};
