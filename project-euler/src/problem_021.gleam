import gleam/int
import gleam/io
import gleam/result
import gleam/yielder
import lib/yielder_utils

const target = 10_000

fn divisor_sum(n: Int) {
  use divisors <- result.try(yielder_utils.divisors(n))

  divisors |> yielder.filter(fn(d) { n != d }) |> yielder.reduce(int.add)
}

pub fn main() {
  yielder.range(1, target - 1)
  |> yielder.filter_map(fn(n) {
    use sum <- result.try(divisor_sum(n))
    use other <- result.try(divisor_sum(sum))

    case n == other && n < sum {
      True -> [n, sum] |> yielder.from_list |> Ok
      _ -> Error(Nil)
    }
  })
  |> yielder.flatten
  |> yielder.reduce(int.add)
  |> result.map(io.debug)
}
