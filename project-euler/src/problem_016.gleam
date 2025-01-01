import gleam/int
import gleam/io
import gleam/list
import gleam/result

const target = 1000

pub fn main() {
  int.bitwise_shift_left(1, target)
  |> int.digits(10)
  |> result.map(list.fold(_, 0, fn(acc, n) { acc + n }))
  |> result.map(io.debug)
}
