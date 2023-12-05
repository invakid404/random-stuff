include Lib.Util;

type map_range = (int, int, int);

type almanac = {
  seeds: list(int),
  maps: list(array(map_range)),
};

let nums_delimiter = " " |> Str.regexp_string;

let parse_nums = nums =>
  Str.split(nums_delimiter, nums)
  |> List.map(String.trim)
  |> List.map(int_of_string);

let seeds_delimiter = ": " |> Str.regexp_string;

let parse_seeds = seeds =>
  switch (Str.split(seeds_delimiter, seeds)) {
  | ["seeds", nums] => parse_nums(nums)
  | _ => assert(false)
  };

let parse_range = range_str =>
  switch (parse_nums(range_str)) {
  | [destination, source, length] => (
      source,
      source + length - 1,
      destination,
    )
  | _ => assert(false)
  };

let map_delimiter = "\n" |> Str.regexp_string;

let compare_ranges =
    (
      (l_start, _y_end, _y_dest): map_range,
      (r_start, _r_end, _r_dest): map_range,
    ) =>
  l_start - r_start;

let pad_ranges = ranges => {
  let current_range = idx =>
    idx == Array.length(ranges) ? (max_int, max_int, max_int) : ranges[idx];

  let range_before = idx => idx == 0 ? ((-1), (-1), (-1)) : ranges[idx - 1];

  let rec pad_ranges_helper = idx =>
    if (idx > Array.length(ranges)) {
      Seq.Nil;
    } else {
      let curr_range = current_range(idx);
      let (curr, _, _) = curr_range;

      let prev_range = range_before(idx);
      let (_, prev, _) = prev_range;

      let l = prev + 1;
      let r = curr - 1;

      let next = Seq.Cons(curr_range, () => pad_ranges_helper(idx + 1));

      l <= r ? Seq.Cons((l, r, l), () => next) : next;
    };

  () => pad_ranges_helper(0);
};

let parse_map = map_str =>
  switch (Str.split(map_delimiter, map_str)) {
  | [_, ...ranges] =>
    List.map(parse_range, ranges)
    |> Array.of_list
    |> (
      ranges => {
        Array.fast_sort(compare_ranges, ranges);

        pad_ranges(ranges) |> Array.of_seq;
      }
    )
  | _ => assert(false)
  };

let parse_maps = maps => List.map(parse_map, maps);

let parts_delimiter = "\n\n" |> Str.regexp_string;

let parse_input = input =>
  Str.split(parts_delimiter, input)
  |> List.map(String.trim)
  |> (
    parts =>
      switch (parts) {
      | [seeds, ...maps] => {
          seeds: parse_seeds(seeds),
          maps: parse_maps(maps),
        }
      | _ => assert(false)
      }
  );

let apply_range = ((l, _, d), n) => {
  let offset = n - l;
  let dest = d + offset;

  dest;
};

let find_range = (ranges, n) => {
  let l = ref(0);
  let r = ref(Array.length(ranges) - 1);
  let res = ref(None);

  while (res^ == None && l^ <= r^) {
    let mid = l^ + (r^ - l^) / 2;
    let (s, e, _d) = ranges[mid];

    if (s > n) {
      r := mid - 1;
    } else if (e < n) {
      l := mid + 1;
    } else {
      res := Some(mid);
    };
  };

  res^;
};

let map_value = (ranges, n) =>
  find_range(ranges, n) |> Option.get |> (idx => apply_range(ranges[idx], n));

let part1 = almanac =>
  almanac.maps
  |> List.fold_left(
       (nums, ranges) => List.map(map_value(ranges), nums),
       almanac.seeds,
     )
  |> L.min;

let rec pair_seeds = seeds =>
  switch (seeds) {
  | []
  | [_] => []
  | [a, b, ...rest] => [(a, a + b - 1), ...pair_seeds(rest)]
  };

let part2 = input => {
  let seed_ranges = pair_seeds(input.seeds);

  let next_ranges = (ranges, l, r, s, e) => {
    let len = r - l + 1;

    range(l, r)
    |> Seq.mapi((idx, range_idx) => {
         let curr_range = ranges[range_idx];
         let (curr_l, curr_r, dest_start) = curr_range;
         let dest_end = dest_start + (curr_r - curr_l);

         let left = idx == 0 ? apply_range(curr_range, s) : dest_start;
         let right = idx == len - 1 ? apply_range(curr_range, e) : dest_end;

         (left, right);
       })
    |> List.of_seq;
  };

  let rec part2_helper = (ranges, acc) =>
    switch (ranges) {
    | [] => acc |> List.map(fst) |> L.min
    | [head, ...rest] =>
      let new_acc =
        acc
        |> List.concat_map(((l, r)) => {
             let left_range = find_range(head, l) |> Option.get;
             let right_range = find_range(head, r) |> Option.get;

             next_ranges(head, left_range, right_range, l, r);
           });

      part2_helper(rest, new_acc);
    };

  part2_helper(input.maps, seed_ranges);
};

let () = {
  let input = read_all("inputs/day05.txt") |> parse_input;

  Printf.printf("%d, %d\n", part1(input), part2(input));
};
