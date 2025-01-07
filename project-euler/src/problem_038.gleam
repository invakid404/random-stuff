import gleam/int
import gleam/io
import gleam/result
import gleam/string
import gleam/yielder
import lib/math
import lib/yielder_utils

// 5 digit numbers would immediately exceed 9 digits
const max_digits = 4

pub fn main() {
  let upper_limit = math.pow(10, max_digits) - 1

  yielder.range(1, upper_limit)
  |> yielder.flat_map(fn(num) {
    yielder_utils.infinite_range(1, 1)
    |> yielder.map(fn(factor) { num * factor })
    |> yielder.map(string.inspect)
    |> yielder.scan("", fn(acc, curr) { acc <> curr })
    |> yielder.take_while(fn(product) { string.length(product) <= 9 })
    |> yielder.filter(fn(product) { string.length(product) == 9 })
    |> yielder.filter(math.is_pandigital_str)
  })
  |> yielder.filter_map(int.parse)
  |> yielder.reduce(int.max)
  |> result.map(io.debug)
}
