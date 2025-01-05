import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/yielder
import lib/math
import lib/result_utils
import lib/yielder_utils

fn digits_upper_limit() {
  let assert Ok(nine_factorial) = math.factorial(9)

  yielder_utils.infinite_range(1, 1)
  |> yielder.take_while(fn(digits) {
    let max_factorial = digits * nine_factorial
    let min = math.pow(10, digits - 1)

    min <= max_factorial
  })
  |> yielder.length
}

pub fn main() {
  let max_digits = digits_upper_limit()
  let max_value = math.pow(10, max_digits) - 1

  yielder.range(3, max_value)
  |> yielder.filter(fn(n) {
    let assert Ok(digits) = int.digits(n, 10)
    let assert Ok(digit_factorial_sum) =
      digits
      |> list.try_map(math.factorial)
      |> result_utils.flat_map(list.reduce(_, int.add))

    n == digit_factorial_sum
  })
  |> yielder.reduce(int.add)
  |> result.map(io.debug)
}
