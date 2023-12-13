include Lib.Util;

Py.initialize();

let np = Py.import("numpy");
let builtins = Py.Eval.get_builtins();

let to_numpy_array = py_list => {
  Py.Module.get_function_with_keywords(
    np,
    "array",
    [|py_list|],
    [("dtype", Py.Dict.find_string(builtins, "int"))],
  );
};

let numpy_diff = (arr, axis) =>
  Py.Module.get_function_with_keywords(
    np,
    "diff",
    [|arr|],
    [("axis", Py.Int.of_int(axis))],
  );

let numpy_all = (arr, axis) =>
  Py.Module.get_function_with_keywords(
    np,
    "all",
    [|arr|],
    [("axis", Py.Int.of_int(axis))],
  );

let numpy_where = arr => Py.Module.get_function(np, "where", [|arr|]);

let numpy_sum = (~axis=None, arr) =>
  Py.Module.get_function_with_keywords(
    np,
    "sum",
    [|arr|],
    switch (axis) {
    | Some(value) => [("axis", Py.Int.of_int(value))]
    | None => []
    },
  );

let numpy_concat = (l, r) =>
  Py.Module.get_function(np, "concatenate", [|Py.Tuple.of_pair((l, r))|]);

let numpy_shape = arr =>
  Py.Object.get_attr_string(arr, "shape") |> Option.get;

let numpy_transpose = arr =>
  Py.Object.get_attr_string(arr, "T") |> Option.get;

let py_nth = (obj, n) => Py.Object.get_item(obj, Py.Int.of_int(n));

let py_compare = (x, y, cmp) => Py.Object.rich_compare(x, y, cmp);

let py_add = Py.Number.add;
let py_sub = Py.Number.subtract;

let py_min = (l, r) =>
  Py.Dict.find_string(builtins, "min")
  |> Py.Callable.to_function(_, [|l, r|]);

let line_delimiter = "\n" |> Str.regexp_string;

let int_of_node =
  fun
  | '.' => 0
  | '#' => 1
  | _ => assert(false);

let parse_mirror =
  Str.split(line_delimiter)
  >> List.map(
       String.to_seq
       >> Seq.map(int_of_node >> Py.Int.of_int)
       >> Py.List.of_seq,
     )
  >> Py.List.of_list
  >> to_numpy_array;

let mirror_delimiter = "\n\n" |> Str.regexp_string;

let parse_input = Str.split(mirror_delimiter) >> List.map(parse_mirror);

let is_reflection = (~smudge=false, mirror, row) => {
  let tolerance = smudge ? 1 : 0;
  let to_top =
    numpy_shape(mirror) |> py_nth(_, 0) |> Option.get |> py_sub(_, row);
  let rows = py_min(row, to_top);

  let min_row = py_sub(row, rows);
  let max_row = py_add(row, rows);

  // Couldn't find a way to do this with the bindings...
  let ctx =
    [
      ("np", np),
      ("mirror", mirror),
      ("row", row),
      ("min_row", min_row),
      ("max_row", max_row),
      ("tolerance", tolerance |> Py.Int.of_int),
    ]
    |> List.map(((k, v)) => (Py.String.of_string(k), v))
    |> Py.Dict.of_bindings;

  Py.Run.eval(
    ~globals=ctx,
    "np.sum(mirror[min_row:row] != mirror[row:max_row][::-1]) == tolerance",
  )
  |> Py.Object.is_true;
};

let find_reflections = (~smudge=false, mirror) => {
  let candidate_rows =
    numpy_diff(mirror, 0)
    |> py_compare(_, Py.Int.of_int(0), EQ)
    |> numpy_all(_, 1)
    |> numpy_where
    |> py_nth(_, 0)
    |> Option.get
    |> py_add(_, Py.Int.of_int(1));

  let candidate_rows =
    smudge
      ? mirror
        |> numpy_diff(_, 0)
        |> py_compare(_, Py.Int.of_int(0), NE)
        |> numpy_sum(~axis=Some(1), _)
        |> py_compare(_, Py.Int.of_int(1), EQ)
        |> numpy_where
        |> py_nth(_, 0)
        |> Option.get
        |> py_add(_, Py.Int.of_int(1))
        |> numpy_concat(candidate_rows)
      : candidate_rows;

  let rec find_reflection = it =>
    switch (Py.Iter.next(it)) {
    | Some(item) =>
      is_reflection(~smudge, mirror, item) ? item : find_reflection(it)
    | None => Py.Int.of_int(-1)
    };
  let it = Py.Object.get_iter(candidate_rows);

  find_reflection(it) |> Py.Int.to_int;
};

let solve_one = (~smudge=false, mirror) => {
  let rows = find_reflections(~smudge, mirror);

  rows >= 0
    ? 100 * rows : find_reflections(~smudge, numpy_transpose(mirror));
};

let solve = (~smudge=false, input) =>
  input |> List.map(solve_one(~smudge)) |> List.fold_left((+), 0);

let part1 = solve(~smudge=false);

let part2 = solve(~smudge=true);

let () = {
  let input = read_all("inputs/day13.txt") |> parse_input;

  Printf.printf("%d, %d\n", part1(input), part2(input));
};
