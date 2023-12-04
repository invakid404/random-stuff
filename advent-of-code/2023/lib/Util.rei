let read_lines: string => Seq.t(string);

let is_digit: char => bool;

let digit_to_int: char => int;

let join: (string, list(string)) => string;

let (^$): (string, char) => string;
let ($^): (char, string) => string;

module S: {
  let for_each: ('a => unit, Seq.t('a)) => unit;
};

module L: {
  let for_each: ('a => unit, list('a)) => unit;
};

let clamp: (int, int, int) => int;

let range: (int, int) => Seq.t(int);

let pow: (int, int) => int;
