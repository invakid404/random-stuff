import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/yielder
import lib/primes
import lib/yielder_utils

const window_length = 3

pub fn main() {
  let digits = [1, 2, 3, 4, 5, 6, 7, 8, 9, 0]

  let num_windows = list.length(digits) - window_length
  let primes = primes.primes() |> yielder.take(num_windows) |> yielder.to_list

  digits
  |> yielder_utils.permutations
  |> yielder.filter(fn(curr) {
    case curr {
      [0, ..] -> False
      _ -> True
    }
  })
  |> yielder.filter(fn(curr) {
    curr
    |> list.drop(1)
    |> list.window(window_length)
    |> list.zip(primes)
    |> list.all(fn(pair) {
      let #(window, p) = pair
      let assert Ok(n) = int.undigits(window, 10)

      n % p == 0
    })
  })
  |> yielder.filter_map(int.undigits(_, 10))
  |> yielder.reduce(int.add)
  |> result.map(io.debug)
}
