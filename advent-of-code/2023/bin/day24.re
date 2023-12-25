include Lib.Util;

type vector = (float, float, float);

let x = ((x, _, _)) => x;
let y = ((_, y, _)) => y;
let z = ((_, _, z)) => z;

let vec_nth = (vec, n) =>
  switch (n) {
  | 0 => vec |> x
  | 1 => vec |> y
  | 2 => vec |> z
  | _ => assert(false)
  };

type hailstone = {
  position: vector,
  velocity: vector,
  slope: float,
  intercept: float,
};

let vector_delimiter = ", " |> Str.regexp_string;

let parse_vector = vector =>
  switch (Str.split(vector_delimiter, vector)) {
  | [x, y, z] => (
      x |> float_of_string,
      y |> float_of_string,
      z |> float_of_string,
    )
  | _ => assert(false)
  };

let hailstone_delimiter = " @ " |> Str.regexp_string;

let parse_hailstone = hailstone =>
  switch (Str.split(hailstone_delimiter, hailstone)) {
  | [position, velocity] =>
    let position = parse_vector(position);
    let velocity = parse_vector(velocity);

    let (px, py, _) = position;
    let (vx, vy, _) = velocity;

    let slope = vy /. vx;
    let intercept = py -. slope *. px;

    {position, velocity, slope, intercept};
  | _ => assert(false)
  };

let parse_input = Seq.map(parse_hailstone) >> Array.of_seq;

let test_area = (200000000000000., 400000000000000.);

let in_test_area = (ix, iy) => {
  let (lo, hi) = test_area;

  lo <= ix && ix <= hi && lo <= iy && iy <= hi;
};

let part1 = input => {
  let total = ref(0);
  let n = Array.length(input);

  for (i in 0 to n - 1) {
    for (j in i + 1 to n - 1) {
      let (h1, h2) = (input[i], input[j]);
      if (h1.slope != h2.slope) {
        let ix = (h2.intercept -. h1.intercept) /. (h1.slope -. h2.slope);
        let iy = h1.slope *. ix +. h1.intercept;

        let t1 = (ix -. (h1.position |> x)) /. (h1.velocity |> x);
        let t2 = (ix -. (h2.position |> x)) /. (h2.velocity |> x);

        if (t1 >= 0. && t2 >= 0. && in_test_area(ix, iy)) {
          total := total^ + 1;
        };
      };
    };
  };

  total^;
};

let part2 = input => {
  let context = Z3.mk_context([]);
  let solver = Z3.Solver.mk_solver(context, None);

  let mk_real = name =>
    Z3.Arithmetic.Real.mk_const(context, Z3.Symbol.mk_string(context, name));

  let coords = Array.init(3, i => mk_real("c" ++ string_of_int(i)));
  let velocities = Array.init(3, i => mk_real("v" ++ string_of_int(i)));
  let times = Array.init(3, i => mk_real("t" ++ string_of_int(i)));

  let add_constraint = (idx, component) => {
    let coord = coords[component];
    let velocity = velocities[component];
    let time = times[idx];

    let hailstone = input[idx];
    let coord_value = vec_nth(hailstone.position, component);
    let velocity_value = vec_nth(hailstone.velocity, component);

    // lhs = coord + (velocity * time)
    let lhs =
      Z3.Arithmetic.mk_add(
        context,
        [coord, Z3.Arithmetic.mk_mul(context, [velocity, time])],
      );

    let coord_value =
      Z3.Arithmetic.Real.mk_numeral_s(
        context,
        coord_value |> Printf.sprintf("%.0f"),
      );
    let velocity_value =
      Z3.Arithmetic.Real.mk_numeral_s(
        context,
        velocity_value |> Printf.sprintf("%.0f"),
      );

    // rhs = coord_value + (velocity_value * time)
    let rhs =
      Z3.Arithmetic.mk_add(
        context,
        [
          coord_value,
          Z3.Arithmetic.mk_mul(context, [velocity_value, time]),
        ],
      );

    // lhs == rhs
    let condition = Z3.Boolean.mk_eq(context, lhs, rhs);

    Z3.Solver.add(solver, [condition]);
  };

  for (idx in 0 to Array.length(times) - 1) {
    for (component in 0 to 2) {
      add_constraint(idx, component);
    };
  };

  switch (Z3.Solver.check(solver, [])) {
  | Z3.Solver.SATISFIABLE =>
    let model = Z3.Solver.get_model(solver) |> Option.get;

    let result = Z3.Arithmetic.mk_add(context, coords |> Array.to_list);

    let result = Z3.Model.eval(model, result, true) |> Option.get;
    // We should end up with a whole number
    assert(
      Z3.Arithmetic.Real.get_denominator(result)
      |> Z3.Arithmetic.Real.numeral_to_string == "1",
    );

    Z3.Arithmetic.Real.to_decimal_string(result, 0) |> int_of_string;
  | _ => assert(false)
  };
};

let () = {
  let input = read_lines("inputs/day24.txt") |> parse_input;

  Printf.printf("%d, %d\n", part1(input), part2(input));
};
