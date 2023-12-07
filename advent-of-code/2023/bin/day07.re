include Lib.Util;

type card =
  | Ace
  | King
  | Queen
  | Jack
  | Number(int);

type hand = list(card);

type hand_type =
  | HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind;

type game = {
  hand,
  bid: int,
};

type game_with_value = {
  hand,
  bid: int,
  value: hand_type,
};

let int_of_hand_type =
  fun
  | HighCard => 1
  | OnePair => 2
  | TwoPair => 3
  | ThreeOfAKind => 4
  | FullHouse => 5
  | FourOfAKind => 6
  | FiveOfAKind => 7;

let card_of_char =
  fun
  | 'A' => Ace
  | 'K' => King
  | 'Q' => Queen
  | 'J' => Jack
  | 'T' => Number(10)
  | card => Number(digit_to_int(card));

let int_of_card =
  fun
  | Ace => 14
  | King => 13
  | Queen => 12
  | Jack => 11
  | Number(n) => n;

let int_of_card_with_jokers =
  fun
  | Jack => 1
  | n => int_of_card(n);

let parse_cards = cards =>
  String.to_seq(cards) |> Seq.map(card_of_char) |> List.of_seq;

let word_delimiter = " " |> Str.regexp_string;

let evaluate_hand = hand => {
  let counts =
    List.fold_left(
      (acc, card) => {
        let current =
          Hashtbl.find_opt(acc, card) |> Option.value(~default=0);
        Hashtbl.replace(acc, card, current + 1);

        acc;
      },
      Hashtbl.create(5),
      hand,
    );

  let occurrences =
    Hashtbl.fold(
      (_, count, acc) => {
        let (pairs, threes, fours, fives) = acc;
        switch (count) {
        | 2 => (pairs + 1, threes, fours, fives)
        | 3 => (pairs, threes + 1, fours, fives)
        | 4 => (pairs, threes, fours + 1, fives)
        | 5 => (pairs, threes, fours, fives + 1)
        | _ => acc
        };
      },
      counts,
      (0, 0, 0, 0),
    );

  switch (occurrences) {
  | (_, _, _, 1) => FiveOfAKind
  | (_, _, 1, _) => FourOfAKind
  | (1, 1, _, _) => FullHouse
  | (_, 1, _, _) => ThreeOfAKind
  | (2, _, _, _) => TwoPair
  | (1, _, _, _) => OnePair
  | _ => HighCard
  };
};

let parse_hand = hand =>
  switch (Str.split(word_delimiter, hand)) {
  | [cards, bid] =>
    let hand = parse_cards(cards);
    {hand, bid: int_of_string(bid)};
  | _ => assert(false)
  };

let parse_input = input => input |> Seq.map(parse_hand) |> List.of_seq;

let compare_hands = (f, left, right) =>
  List.map2((l, r) => f(l) - f(r), left, right)
  |> List.find(value => value != 0);

let compare_games = (f, left, right) => {
  let left_value = int_of_hand_type(left.value);
  let right_value = int_of_hand_type(right.value);

  let delta = left_value - right_value;

  delta != 0 ? delta : compare_hands(f, left.hand, right.hand);
};

let winnings = (f, input) =>
  input
  |> List.fast_sort(compare_games(f))
  |> List.mapi((idx, game) => game.bid * (idx + 1))
  |> List.fold_left((+), 0);

let evaluate_game: (hand => hand_type, game) => game_with_value =
  (f, game) => {hand: game.hand, bid: game.bid, value: f(game.hand)};

let part1 = input =>
  input |> List.map(evaluate_game(evaluate_hand)) |> winnings(int_of_card);

let has_jokers = hand =>
  hand |> List.find_opt(card => card == Jack) |> Option.is_some;

let replace_jokers = (hand, card) =>
  hand |> List.map(curr => curr == Jack ? card : curr);

let joker_replacements =
  "23456789TQKA" |> String.to_seq |> Seq.map(card_of_char);

let evaluate_hand_with_jokers = hand =>
  has_jokers(hand)
    ? joker_replacements
      |> Seq.map(replace_jokers(hand))
      |> Seq.map(evaluate_hand)
      |> Seq.fold_left(max, HighCard)
    : evaluate_hand(hand);

let part2 = input =>
  input
  |> List.map(evaluate_game(evaluate_hand_with_jokers))
  |> winnings(int_of_card_with_jokers);

let () = {
  let input = read_lines("inputs/day07.txt") |> parse_input;

  Printf.printf("%d, %d\n", part1(input), part2(input));
};
