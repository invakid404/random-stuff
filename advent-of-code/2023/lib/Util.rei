let read_all: string => string;

let read_lines: string => Seq.t(string);

let is_digit: char => bool;

let digit_to_int: char => int;

let join: (string, list(string)) => string;

let (^$): (string, char) => string;
let ($^): (char, string) => string;

module S: {
  let for_each: ('a => unit, Seq.t('a)) => unit;

  let scan_left: (('b, 'a) => 'b, 'b, Seq.t('a)) => Seq.t('b);

  let scan_left1: (('a, 'a) => 'a, Seq.t('a)) => Seq.t('a);
};

module L: {
  let for_each: ('a => unit, list('a)) => unit;

  let min: list('a) => 'a;
  let max: list('a) => 'a;

  let zip: (list('a), list('b)) => list(('a, 'b));

  let unique: list('a) => list('a);

  let upsert_first: ('a => bool, 'a, list('a)) => list('a);
};

module A: {
  let transpose: array(array('a)) => array(array('a));
};

let clamp: (int, int, int) => int;

let range: (int, int) => Seq.t(int);

let pow: (int, int) => int;

let int_length: int => int;

let int_concat: (int, int) => int;

let gcd: (int, int) => int;
let lcm: (int, int) => int;

let (>>): ('a => 'b, 'b => 'c, 'a) => 'c;
