include Lib.Util;

type state =
  | Operational
  | Damaged
  | Unknown;

let state_of_char =
  fun
  | '.' => Operational
  | '#' => Damaged
  | '?' => Unknown
  | _ => assert(false);

let char_of_state =
  fun
  | Operational => '.'
  | Damaged => '#'
  | Unknown => '?';

type spring_row = {
  state: array(state),
  groups: array(int),
};

type springs = list(spring_row);

let parse_state = String.to_seq >> Seq.map(state_of_char) >> Array.of_seq;

let parse_groups =
  String.split_on_char(',') >> List.map(int_of_string) >> Array.of_list;

let parse_row = row =>
  switch (String.split_on_char(' ', row)) {
  | [state, groups] => {
      state: parse_state(state),
      groups: parse_groups(groups),
    }
  | _ => assert(false)
  };

let parse_input = Seq.map(parse_row) >> List.of_seq;

let arrangements = ({state, groups}) => {
  let get_state = idx =>
    idx == 0
      ? Some(Operational)
      : idx == Array.length(state) + 1
          ? Some(Operational)
          : idx <= Array.length(state) ? Some(state[idx - 1]) : None;

  let as_operational = (next, group_idx, current_group) =>
    if (current_group == 0) {
      next(group_idx, 0);
    } else if (group_idx < Array.length(groups)
               && current_group == groups[group_idx]) {
      next(group_idx + 1, 0);
    } else {
      0;
    };

  let as_damaged = (next, group_idx, current_group) =>
    group_idx < Array.length(groups) && current_group < groups[group_idx]
      ? next(group_idx, current_group + 1) : 0;

  let evaluate_one = (next, curr_state, group_idx, current_group) => {
    switch (curr_state) {
    | Operational => as_operational(next, group_idx, current_group)
    | Damaged => as_damaged(next, group_idx, current_group)
    | Unknown =>
      let operational = as_operational(next, group_idx, current_group);
      let damaged = as_damaged(next, group_idx, current_group);

      operational + damaged;
    };
  };

  let memo =
    Hashtbl.create(
      Array.length(state) * Array.length(groups) * Array.length(groups),
    );

  let rec arrangements_helper = (state_idx, group_idx, current_group) => {
    switch (get_state(state_idx)) {
    | Some(curr_state) =>
      let memo_key = (state_idx, group_idx, current_group);
      let memoized_result = Hashtbl.find_opt(memo, memo_key);

      switch (memoized_result) {
      | Some(value) => value
      | None =>
        let next = arrangements_helper(state_idx + 1);
        let result = evaluate_one(next, curr_state, group_idx, current_group);

        Hashtbl.add(memo, memo_key, result);

        result;
      };
    | None => group_idx == Array.length(groups) && current_group == 0 ? 1 : 0
    };
  };

  arrangements_helper(0, 0, 0);
};

let part1 = List.map(arrangements) >> List.fold_left((+), 0);

let unfold_state = (n, arr) => {
  let original_length = Array.length(arr);
  let new_length = original_length * n + (n - 1);
  let result = Array.make(new_length, Unknown);

  for (i in 0 to n - 1) {
    let offset = i * (original_length + 1);
    for (j in 0 to original_length - 1) {
      result[offset + j] = arr[j];
    };
  };

  result;
};

let unfold_groups = (n, arr) => {
  let original_length = Array.length(arr);
  let new_length = n * original_length;
  let result = Array.make(n * original_length, 0);

  for (i in 0 to new_length - 1) {
    result[i] = arr[i mod original_length];
  };

  result;
};

let unfold = row => {
  {state: unfold_state(5, row.state), groups: unfold_groups(5, row.groups)};
};

let part2 = List.map(unfold) >> part1;

let () = {
  let input = read_lines("inputs/day12.txt") |> parse_input;

  Printf.printf("%d, %d\n", part1(input), part2(input));
};
