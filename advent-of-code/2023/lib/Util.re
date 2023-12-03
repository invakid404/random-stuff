let read_lines = file => {
  let input = open_in(file);
  let rec collect_lines = () => {
    switch (input_line(input)) {
    | line => Seq.Cons(line, collect_lines)
    | exception End_of_file =>
      close_in(input);
      Seq.Nil;
    };
  };

  collect_lines;
};

let is_digit = c => c >= '0' && c <= '9';

let digit_to_int = digit => Char.code(digit) - Char.code('0');

let join = (char, list) => {
  let rec join_helper = (~acc="", list) =>
    switch (list) {
    | [] => acc
    | [tail] => acc ++ tail
    | [head, ...tail] => join_helper(tail, ~acc=acc ++ head ++ char)
    };

  join_helper(list);
};

let (^$) = (s, c) => s ++ String.make(1, c);
let ($^) = (c, s) => String.make(1, c) ++ s;

module S = {
  let for_each = (f, seq) => {
    let _ =
      Seq.for_all(
        value => {
          f(value);

          true;
        },
        seq,
      );

    ();
  };
};

module L = {
  let for_each = (f, seq) => {
    let _ = List.map(f, seq);

    ();
  };
};

let clamp = (l, r, x) => min(max(x, l), r);

exception InvalidRange(int, int);

let range = (l, r) => {
  if (l > r) {
    raise(InvalidRange(l, r));
  };

  let rec range_helper = i =>
    if (i <= r) {
      Seq.Cons(i, () => range_helper(i + 1));
    } else {
      Seq.Nil;
    };
  ();

  () => range_helper(l);
};
