import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/yielder
import lib/math

pub fn main() {
  yielder.range(11, 99)
  |> yielder.filter(fn(a) { a % 10 != 0 })
  |> yielder.flat_map(fn(a) {
    let assert Ok(digits) = int.digits(a, 10)
    digits
    |> yielder.from_list
    |> yielder.flat_map(fn(matching_digit) {
      let remaining =
        digits
        |> list.filter(fn(digit) { digit != matching_digit })
        |> list.first
        |> result.unwrap(matching_digit)

      yielder.range(1, 9)
      |> yielder.flat_map(fn(other_digit) {
        let digit_cancelled_fraction =
          math.make_fraction(remaining, other_digit)

        [
          #(
            math.make_fraction(a, matching_digit * 10 + other_digit),
            digit_cancelled_fraction,
          ),
          ..{
            case matching_digit != other_digit {
              True -> [
                #(
                  math.make_fraction(a, other_digit * 10 + matching_digit),
                  digit_cancelled_fraction,
                ),
              ]
              False -> []
            }
          }
        ]
        |> yielder.from_list
      })
    })
    |> yielder.filter(fn(fraction_pair) {
      math.numerator(fraction_pair.0) < math.denominator(fraction_pair.0)
    })
    |> yielder.map(fn(fraction_pair) {
      #(
        fraction_pair.0 |> math.simplify_fraction,
        fraction_pair.1 |> math.simplify_fraction,
      )
    })
    |> yielder.filter(fn(fraction_pair) { fraction_pair.0 == fraction_pair.1 })
  })
  |> yielder.map(fn(fraction_pair) { fraction_pair.0 })
  |> yielder.reduce(math.multiply_fractions)
  |> result.map(math.denominator)
  |> result.map(io.debug)
}
