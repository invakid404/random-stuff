include Lib.Util;

type card = {
  id: int,
  winning_numbers: Hashtbl.t(int, unit),
  numbers: list(int),
};

let word_delimiter = "[ \t]+" |> Str.regexp;

let numbers_delimiter = "|" |> Str.regexp_string;

let split_numbers = s =>
  Str.split(word_delimiter, s)
  |> List.map(String.trim)
  |> List.map(int_of_string);

let parse_winning_numbers = winning_numbers_str => {
  let nums = split_numbers(winning_numbers_str);
  let map = Hashtbl.create(List.length(nums));

  List.to_seq(nums) |> Seq.map(num => (num, ())) |> Hashtbl.add_seq(map);

  map;
};

let parse_own_numbers = split_numbers;

let parse_numbers = numbers_str =>
  switch (Str.split(numbers_delimiter, numbers_str)) {
  | [winning_numbers_str, numbers_str] => (
      parse_winning_numbers(winning_numbers_str),
      parse_own_numbers(numbers_str),
    )
  | _ => assert(false)
  };

let parse_id = card_id =>
  switch (Str.split(word_delimiter, card_id)) {
  | ["Card", id_str] => int_of_string(id_str)
  | _ => assert(false)
  };

let card_delimiter = ":" |> Str.regexp_string;

let parse_card = line =>
  switch (Str.split(card_delimiter, line)) {
  | [id_str, numbers_str] =>
    let id = parse_id(id_str);
    let (winning_numbers, numbers) = parse_numbers(numbers_str);

    {id, winning_numbers, numbers};
  | _ => assert(false)
  };

let winning_numbers = card =>
  card.numbers |> List.filter(Hashtbl.mem(card.winning_numbers));

let part1 = cards =>
  cards
  |> List.map(card => {
       let winning_amt = winning_numbers(card) |> List.length;

       winning_amt > 0 ? pow(2, winning_amt - 1) : 0;
     })
  |> List.fold_left((+), 0);

let part2 = cards => {
  let amounts = Hashtbl.create(List.length(cards));
  cards
  |> List.to_seq
  |> Seq.map(card => (card.id, 1))
  |> Hashtbl.add_seq(amounts);

  L.for_each(
    card => {
      let winning_amt = winning_numbers(card) |> List.length;
      let amount = Hashtbl.find(amounts, card.id);

      if (winning_amt > 0) {
        range(card.id + 1, card.id + winning_amt)
        |> S.for_each(id =>
             Hashtbl.replace(amounts, id, Hashtbl.find(amounts, id) + amount)
           );
      };
    },
    cards,
  );

  Hashtbl.to_seq_values(amounts) |> Seq.fold_left((+), 0);
};

let () = {
  let cards =
    read_lines("./inputs/day04.txt") |> Seq.map(parse_card) |> List.of_seq;

  Printf.printf("%d, %d\n", part1(cards), part2(cards));
};
