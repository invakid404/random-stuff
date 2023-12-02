let read_lines: string => Seq.t(string);

let digit_to_int: char => int;

let join: (string, list(string)) => string;

let (^$): (string, char) => string;
let ($^): (char, string) => string;

module S: {
  let for_each: ('a => unit, Seq.t('a)) => unit;
};
