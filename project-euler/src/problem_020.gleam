import gleam/int
import gleam/io
import gleam/list
import gleam/result
import lib/math.{factorial}
import lib/result_utils

const target = 100

pub fn main() {
  factorial(target)
  |> result_utils.flat_map(int.digits(_, 10))
  |> result_utils.flat_map(list.reduce(_, int.add))
  |> result.map(io.debug)
}
