import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import lib/file

const input_filename = "src/problem_013.txt"

pub fn main() {
  let assert Ok(input) = file.read(input_filename) |> result.map(string.trim)
  let assert Ok(input) = string.split(input, "\n") |> list.try_map(int.parse)

  let assert Ok(result) = list.reduce(input, fn(acc, n) { acc + n })

  string.inspect(result) |> string.slice(0, 10) |> io.println
}
