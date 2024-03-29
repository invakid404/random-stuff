let read_all = file => {
  let input = open_in(file);
  let res = really_input_string(input, in_channel_length(input));

  close_in(input);
  res;
};

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

  let scan_left = (f, initial, seq) => {
    let rec scan_left_helper = (acc, seq) =>
      switch (Seq.uncons(seq)) {
      | None => Seq.Nil
      | Some((head, rest)) =>
        let next = f(acc, head);
        Seq.Cons(next, () => scan_left_helper(next, rest));
      };

    () => scan_left_helper(initial, seq);
  };

  let scan_left1 = (f, seq) => {
    switch (Seq.uncons(seq)) {
    | None => (() => Seq.Nil)
    | Some((head, _)) => scan_left(f, head, seq)
    };
  };
};

module L = {
  let for_each = (f, lst) => {
    let _ = List.map(f, lst);

    ();
  };

  exception EmptyList;

  let min = lst =>
    switch (lst) {
    | [] => raise(EmptyList)
    | [head, ...rest] => List.fold_left(min, head, rest)
    };

  let max = lst =>
    switch (lst) {
    | [] => raise(EmptyList)
    | [head, ...rest] => List.fold_left(max, head, rest)
    };

  let zip = (l, r) => {
    let rec zip_helper = (l, r, acc) =>
      switch (l, r) {
      | ([l_head, ...l_rest], [r_head, ...r_rest]) =>
        zip_helper(l_rest, r_rest, [(l_head, r_head), ...acc])
      | _ => List.rev(acc)
      };

    zip_helper(l, r, []);
  };

  let unique = lst => {
    let tbl = Hashtbl.create(List.length(lst));
    for_each(x => {Hashtbl.replace(tbl, x, ())}, lst);

    Hashtbl.to_seq_keys(tbl) |> List.of_seq;
  };

  let upsert_first = (predicate, new_value, list) => {
    let rec upsert_helper = (acc, lst) =>
      switch (lst) {
      | [] => List.rev([new_value, ...acc])
      | [head, ...tail] =>
        if (predicate(head)) {
          List.rev(acc) @ [new_value, ...tail];
        } else {
          upsert_helper([head, ...acc], tail);
        }
      };

    upsert_helper([], list);
  };
};

module A = {
  let transpose = matrix => {
    let rows = Array.length(matrix);
    let cols = rows > 0 ? Array.length(matrix[0]) : 0;

    let transposed = Array.make(cols, [||]);

    for (i in 0 to cols - 1) {
      let new_row = Array.make(rows, matrix[0][0]);
      for (j in 0 to rows - 1) {
        new_row[j] = matrix[j][i];
      };

      transposed[i] = new_row;
    };

    transposed;
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

let rec pow = (a, n) =>
  switch (n) {
  | 0 => 1
  | 1 => a
  | _ =>
    let b = pow(a, n / 2);
    b * b * (n mod 2 == 0 ? 1 : a);
  };

let int_length = n => truncate(log10(float_of_int(n))) + 1;

let int_concat = (x, y) => x * pow(10, int_length(y)) + y;

let rec gcd = (u, v) =>
  if (v != 0) {
    gcd(v, u mod v);
  } else {
    abs(u);
  };

let lcm = (m, n) =>
  switch (m, n) {
  | (0, _)
  | (_, 0) => 0
  | (m, n) => abs(m * n) / gcd(m, n)
  };

let (>>) = (f, g, x) => g(f(x));
