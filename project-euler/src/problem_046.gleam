import gleam/io
import gleam/result
import gleam/yielder
import lib/primes
import lib/yielder_utils

fn goldbach(n: Int) {
  yielder_utils.infinite_range(1, 1)
  |> yielder.map(fn(s) { 2 * s * s })
  |> yielder.take_while(fn(s) { s < n })
  |> yielder.find(fn(s) {
    let delta = n - s

    primes.is_prime(delta)
  })
}

pub fn main() {
  yielder_utils.infinite_range(9, 2)
  |> yielder.filter(fn(n) { !primes.is_prime(n) })
  |> yielder.find(fn(n) { goldbach(n) |> result.is_error })
  |> result.map(io.debug)
}
