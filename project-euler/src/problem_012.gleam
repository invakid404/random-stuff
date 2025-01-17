import gleam/io
import gleam/result
import gleam/yielder
import lib/math.{triangle_numbers}
import lib/yielder_utils.{n_divisors}

pub fn main() {
  triangle_numbers()
  |> yielder.find(fn(n) {
    let assert Ok(divisors) = n_divisors(n)

    divisors > 500
  })
  |> result.map(io.debug)
}
