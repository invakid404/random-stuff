import gleam/int
import gleam/io
import gleam/yielder

fn flip_digits(value: Int) {
  flip_digits_helper(value, 0)
}

fn flip_digits_helper(value: Int, acc: Int) {
  case value {
    0 -> acc
    _ -> flip_digits_helper(value / 10, acc * 10 + value % 10)
  }
}

fn is_palindrome(value: Int) {
  value == flip_digits(value)
}

pub fn main() {
  let assert Ok(result) =
    yielder.range(100, 999)
    |> yielder.flat_map(fn(value) {
      yielder.map2(yielder.repeat(value), yielder.range(value, 999), fn(a, b) {
        a * b
      })
      |> yielder.filter(is_palindrome)
    })
    |> yielder.reduce(fn(acc, curr) { int.max(acc, curr) })

  io.debug(result)
}
