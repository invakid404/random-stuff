import file_streams/file_stream
import gleam/bit_array
import gleam/int
import gleam/io
import gleam/list
import gleam/string

const window_size = 13

pub fn main() {
  let input_filename = "src/problem_008.txt"
  let assert Ok(stream) = file_stream.open_read(input_filename)

  let assert Ok(input) = file_stream.read_remaining_bytes(stream)
  let assert Ok(input) = bit_array.to_string(input)
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
