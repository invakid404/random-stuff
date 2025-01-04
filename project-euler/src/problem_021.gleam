import gleam/int
import gleam/io
import gleam/result
import gleam/yielder
import lib/yielder_utils.{proper_divisors_sum}

const target = 10_000

pub fn main() {
  yielder.range(1, target - 1)
  |> yielder.filter_map(fn(n) {
    use sum <- result.try(proper_divisors_sum(n))
    use other <- result.try(proper_divisors_sum(sum))

    case n == other && n < sum {
      True -> [n, sum] |> yielder.from_list |> Ok
      _ -> Error(Nil)
    }
  })
  |> yielder.flatten
  |> yielder.reduce(int.add)
  |> result.map(io.debug)
}
