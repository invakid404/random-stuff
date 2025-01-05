import gleam/int
import gleam/io
import gleam/yielder
import lib/list_utils
import lib/primes

const upper_limit = 1_000_000

pub fn main() {
  let upper_limit = upper_limit - 1

  // Prime the cache (pun not intended)
  primes.is_prime(upper_limit)

  yielder.range(2, upper_limit)
  |> yielder.filter(primes.is_prime)
  |> yielder.filter(fn(p) {
    let assert Ok(digits) = int.digits(p, 10)

    list_utils.rotations(digits)
    |> yielder.all(fn(curr) {
      let assert Ok(curr) = int.undigits(curr, 10)

      primes.is_prime(curr)
    })
  })
  |> yielder.length
  |> io.debug
}
