import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/yielder
import lib/yielder_utils
import precompute_primes.{is_prime, load_primes}

pub fn main() {
  let assert Ok(primes) = load_primes()

  yielder.iterate([9, 8, 7, 6, 5, 4, 3, 2, 1], list.drop(_, 1))
  |> yielder.take_while(fn(curr) { curr != [] })
  |> yielder.filter_map(fn(curr) {
    yielder_utils.permutations(curr)
    |> yielder.filter_map(int.undigits(_, 10))
    |> yielder.find(is_prime(primes, _))
  })
  |> yielder.first
  |> result.map(io.debug)
}
