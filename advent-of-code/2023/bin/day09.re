include Lib.Util;

let parse_nums = line =>
  String.split_on_char(' ', line) |> List.map(int_of_string);

let deltas = nums =>
  L.zip(List.tl(nums), nums) |> List.map(((a, b)) => a - b);

let rec find_next = nums =>
  List.for_all(n => n == 0, nums)
    ? 0
    : {
      let last = nums |> List.rev |> List.hd;
      let next = deltas(nums) |> find_next;

      last + next;
    };

let part1 = List.map(find_next) >> List.fold_left((+), 0);

let part2 = List.map(List.rev) >> part1;

let () = {
  let input =
    read_lines("inputs/day09.txt") |> Seq.map(parse_nums) |> List.of_seq;

  Printf.printf("%d, %d\n", part1(input), part2(input));
};
