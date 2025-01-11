import gleam/io
import gleam/list
import gleam/string
import gleam/yielder
import lib/file
import lib/math

const target = "src/problem_042.txt"

pub fn main() {
  let assert Ok(input) = file.read(target)

  let n = string.length(input)
  let input = input |> string.slice(1, n - 2) |> string.split("\",\"")

  input
  |> yielder.from_list
  |> yielder.fold(0, fn(acc, str) {
    let n =
      str
      |> string.to_utf_codepoints
      |> list.fold(0, fn(acc, c) { string.utf_codepoint_to_int(c) - 64 + acc })

    case math.is_triangle_number(n) {
      Ok(True) -> acc + 1
      _ -> acc
    }
  })
  |> io.debug
}
