import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/yielder

const target = 1_000_000

fn is_palindrome(n: Int, b: Int) {
  let assert Ok(digits) = int.digits(n, b)

  digits == list.reverse(digits)
}

pub fn main() {
  yielder.range(1, target - 1)
  |> yielder.filter(is_palindrome(_, 10))
  |> yielder.filter(is_palindrome(_, 2))
  |> yielder.reduce(int.add)
  |> result.map(io.debug)
}
