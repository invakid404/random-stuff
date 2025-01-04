import gleam/int
import gleam/io
import gleam/option
import gleam/result
import gleam/yielder
import glearray
import lib/dict
import lib/yielder_utils.{proper_divisors_sum}

const upper_limit = 28_123

pub fn main() {
  let abundant_nums =
    yielder.range(1, upper_limit)
    |> yielder.filter_map(fn(n) {
      use sum <- result.try(proper_divisors_sum(n))

      case sum > n {
        True -> Ok(n)
        False -> Error(Nil)
      }
    })
    |> yielder.to_list
    |> glearray.from_list

  let n = abundant_nums |> glearray.length

  yielder.range(0, n - 2)
  |> yielder.each(fn(i) {
    let assert Ok(left) = glearray.get(abundant_nums, i)

    yielder.range(i, n - 1)
    |> yielder.all(fn(j) {
      let assert Ok(right) = glearray.get(abundant_nums, j)

      let sum = left + right
      let result = sum <= upper_limit

      case result {
        True -> dict.put(sum, True)
        _ -> Nil
      }

      result
    })
  })

  yielder.range(0, upper_limit)
  |> yielder.filter(fn(n) { dict.get(n) |> option.is_none })
  |> yielder.reduce(int.add)
  |> result.map(io.debug)
}
