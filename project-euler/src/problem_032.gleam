import gleam/io
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import gleam/yielder
import lib/dict
import lib/math
import lib/yielder_utils

// Products cannot:
// - exceed 4 digits (we need the rest of the digits for the factors)
// - start with a 9 (no valid factors make a product starting with 9)
const upper_bound = 8976

type SeenProduct {
  SeenProduct(Int)
}

pub fn main() {
  yielder.range(2, 98)
  |> yielder.flat_map(fn(left) {
    yielder_utils.infinite_range(left + 1, 1)
    |> yielder.map(fn(right) { #(left, right, left * right) })
    |> yielder.take_while(fn(curr) { curr.2 < upper_bound })
    |> yielder.filter(fn(curr) {
      [curr.0, curr.1, curr.2]
      |> list.map(string.inspect)
      |> string.concat
      |> math.is_pandigital_str
    })
  })
  |> yielder.map(fn(curr) { curr.2 })
  |> yielder.reduce(fn(acc, product) {
    let key = SeenProduct(product)
    case dict.get(key) |> option.is_none {
      True -> {
        dict.put(key, True)

        acc + product
      }
      False -> acc
    }
  })
  |> result.map(io.debug)
}
