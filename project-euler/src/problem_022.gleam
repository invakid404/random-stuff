import gleam/io
import gleam/list
import gleam/result
import gleam/string
import lib/file

const target = "src/problem_022.txt"

pub fn main() {
  let assert Ok(input) = file.read(target) |> result.map(string.trim)
  let input =
    string.split(input, ",")
    |> list.map(fn(name) { string.slice(name, 1, string.length(name) - 2) })
    |> list.sort(string.compare)

  input
  |> list.index_fold(0, fn(acc, name, index) {
    let score =
      string.to_utf_codepoints(name)
      |> list.fold(0, fn(acc, char) {
        acc + string.utf_codepoint_to_int(char) - 0x40
      })
    let score = score * { index + 1 }

    acc + score
  })
  |> io.debug
}
