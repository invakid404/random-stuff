import gleam/io
import gleam/result
import gleam/string
import gleam/yielder
import lib/list_utils.{nth_permutation}

const target = 1_000_000

pub fn main() {
  nth_permutation(
    yielder.range(0, 9) |> yielder.map(string.inspect) |> yielder.to_list,
    target,
  )
  |> result.map(string.concat)
  |> result.map(io.println)
}
