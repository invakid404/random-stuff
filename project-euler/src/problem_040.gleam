import gleam/int
import gleam/io
import gleam/result
import gleam/string

fn find_digits(targets: List(Int)) {
  find_digits_helper(1, 1, 0, 1, targets)
}

fn find_digits_helper(
  current_num: Int,
  product: Int,
  current_length: Int,
  next_target: Int,
  targets: List(Int),
) -> Int {
  case targets {
    [] -> product
    [target, ..rest] -> {
      let num_str = int.to_string(current_num)
      let num_len = string.length(num_str)

      case current_length + num_len >= target {
        True -> {
          let digit_pos = target - current_length - 1
          let digit =
            string.slice(num_str, digit_pos, 1)
            |> int.parse
            |> result.unwrap(1)
          find_digits_helper(
            current_num + 1,
            product * digit,
            current_length + num_len,
            next_target * 10,
            rest,
          )
        }
        False ->
          find_digits_helper(
            current_num + 1,
            product,
            current_length + num_len,
            next_target,
            targets,
          )
      }
    }
  }
}

const targets = [1, 10, 100, 1000, 10_000, 100_000, 1_000_000]

pub fn main() {
  find_digits(targets) |> io.debug
}
