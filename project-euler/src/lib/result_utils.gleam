import gleam/result

pub fn flat_map(over result: Result(a, e), with fun: fn(a) -> Result(c, e)) {
  result.map(result, fun) |> result.flatten
}
