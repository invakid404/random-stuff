import gleam/io
import gleam/result
import gleam/yielder
import lib/math.{hexagonal_numbers, pentagonal_numbers, triangle_numbers}
import lib/yielder_utils

const known = 40_755

pub fn main() {
  let drop = fn(in: yielder.Yielder(Int)) {
    in |> yielder.drop_while(fn(curr) { curr <= known })
  }

  yielder_utils.merge_non_decreasing(
    triangle_numbers() |> drop,
    pentagonal_numbers() |> drop,
  )
  |> yielder_utils.merge_non_decreasing(hexagonal_numbers() |> drop)
  |> yielder_utils.filter_repeating(3)
  |> yielder.first
  |> result.map(io.debug)
}
