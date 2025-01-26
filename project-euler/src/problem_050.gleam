import gleam/int
import gleam/io
import gleam/option.{None, Some}
import gleam/result
import gleam/yielder
import glearray
import lib/dict
import lib/primes.{primes}

const target = 1_000_000

pub fn main() {
  let prime_sums =
    primes()
    // Memoize primes up to target
    |> yielder.take_while(fn(p) { p <= target })
    |> yielder.map(fn(p) {
      dict.put(p, True)
      p
    })
    |> yielder.to_list
    |> yielder.from_list
    // Compute sums <= target
    |> yielder.scan(0, int.add)
    |> yielder.prepend(0)
    |> yielder.take_while(fn(sum) { sum <= target })
    |> yielder.to_list
    |> glearray.from_list

  let length = prime_sums |> glearray.length

  yielder.range(0, length - 2)
  |> yielder.flat_map(fn(left) {
    let assert Ok(left_val) = prime_sums |> glearray.get(left)

    yielder.range(left + 1, length - 1)
    |> yielder.filter_map(fn(right) {
      let assert Ok(right_val) = prime_sums |> glearray.get(right)
      let sum = right_val - left_val

      case dict.get(sum) {
        Some(_) -> Ok(#(sum, right - left))
        None -> Error(Nil)
      }
    })
  })
  |> yielder.reduce(fn(acc, curr) {
    case acc.1 < curr.1 {
      True -> curr
      False -> acc
    }
  })
  |> result.map(fn(result) { result.0 })
  |> result.map(io.debug)
}
