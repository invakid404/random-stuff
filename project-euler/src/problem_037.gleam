import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/result
import gleam/yielder
import lib/dict
import lib/math
import lib/primes.{primes}

const n_result = 11

type KnownPrime {
  KnownPrime(Int)
}

pub fn main() {
  let is_prime = fn(n: Int) { dict.get(KnownPrime(n)) |> option.is_some }

  let left_to_right = fn(n: Int) {
    let assert Ok(digits) = int.digits(n, 10)

    digits
    |> list.reverse
    |> yielder.from_list
    |> yielder.index
    |> yielder.map(fn(curr) { curr.0 * math.pow(10, curr.1) })
    |> yielder.scan(0, fn(acc, curr) { acc + curr })
    |> yielder.all(is_prime)
  }

  let right_to_left = fn(n: Int) {
    yielder.iterate(n, fn(curr) { curr / 10 })
    |> yielder.drop(1)
    |> yielder.take_while(fn(curr) { curr > 0 })
    |> yielder.all(is_prime)
  }

  primes()
  |> yielder.map(fn(prime) {
    // Memoize primes as we go
    dict.put(KnownPrime(prime), Nil)

    prime
  })
  // Single-digit primes are not considered truncatable
  |> yielder.filter(fn(prime) { prime >= 11 })
  |> yielder.filter(right_to_left)
  |> yielder.filter(left_to_right)
  |> yielder.take(n_result)
  |> yielder.reduce(int.add)
  |> result.map(io.debug)
}
