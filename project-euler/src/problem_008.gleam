import gleam/int
import gleam/io
import gleam/list
import gleam/string
import lib/file

const window_size = 13

const input_filename = "src/problem_008.txt"

pub fn main() {
  let assert Ok(input) = file.read(input_filename)
  let assert Ok(input) =
    input
    |> string.trim
    |> string.replace("\n", "")
    |> string.to_graphemes
    |> list.try_map(int.parse)

  let assert Ok(result) =
    input
    |> list.window(window_size)
    |> list.filter_map(list.reduce(_, fn(acc, value) { acc * value }))
    |> list.reduce(fn(acc, value) { int.max(acc, value) })

  io.debug(result)
}
