import gleam/io
import gleam/result
import gleam/yielder
import lib/yielder_utils.{n_divisors}

fn triangle_numbers() {
  yielder.unfold(1, fn(value) { yielder.Next(value, value + 1) })
  |> yielder.scan(0, fn(acc, value) { acc + value })
}

pub fn main() {
  triangle_numbers()
  |> yielder.find(fn(n) {
    let assert Ok(divisors) = n_divisors(n)

    divisors > 500
  })
  |> result.map(io.debug)
}
