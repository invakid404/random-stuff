import gleam/int
import gleam/io
import gleam/list
import gleam/option.{Some}
import gleam/result
import gleam/yielder
import lib/dict
import lib/math
import lib/result_utils

// 10^(d - 1) <= d * 9^5 <= 10^d
// ...
// d <= 6.47 < d + 1
// => upper_bound = 6 * 9^5
const upper_bound = 354_294

const power = 5

type DigitPower {
  DigitPower(Int)
}

fn digit_power(d: Int) {
  case d {
    _ if d < 0 || d > 9 -> Error(Nil)
    _ -> {
      let key = DigitPower(d)

      case dict.get(key) {
        Some(result) -> result
        _ -> {
          let result = math.pow(d, power)
          dict.put(key, result)

          result
        }
      }
      |> Ok
    }
  }
}

fn digit_power_sum(n: Int) {
  use digits <- result.try(int.digits(n, 10))

  digits
  |> list.try_map(digit_power)
  |> result_utils.flat_map(list.reduce(_, int.add))
}

pub fn main() {
  yielder.range(10, upper_bound)
  |> yielder.filter_map(fn(num) {
    use sum <- result.try(digit_power_sum(num))
    case num == sum {
      True -> Ok(num)
      False -> Error(Nil)
    }
  })
  |> yielder.reduce(int.add)
  |> result.map(io.debug)
}
