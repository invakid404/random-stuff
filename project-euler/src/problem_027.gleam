import gleam/io
import gleam/result
import gleam/yielder
import lib/math
import lib/primes.{is_prime}
import lib/yielder_utils

const limit = 1000

pub fn main() {
  yielder.range(-limit + 1, limit - 1)
  |> yielder.flat_map(fn(a) {
    yielder.range(-limit, limit)
    |> yielder.map(fn(b) {
      #(
        a,
        b,
        yielder_utils.infinite_range(0, 1)
          |> yielder.take_while(fn(x) {
            let value = math.pow(x, 2) + a * x + b

            is_prime(value)
          })
          |> yielder.length,
      )
    })
  })
  |> yielder.filter(fn(curr) { curr.2 != 0 })
  |> yielder.reduce(fn(max, curr) {
    case max.2 < curr.2 {
      True -> curr
      False -> max
    }
  })
  |> result.map(fn(res) { res.0 * res.1 })
  |> result.map(io.debug)
}
