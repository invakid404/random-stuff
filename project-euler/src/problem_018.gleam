import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import lib/file

const target = "src/problem_018.txt"

fn max_path_sum(triangle: List(List(Int))) -> Int {
  max_path_sum_helper(list.reverse(triangle))
}

fn max_path_sum_helper(reversed_triangle: List(List(Int))) -> Int {
  case reversed_triangle {
    [] -> 0
    [last] -> list.first(last) |> result.unwrap(0)
    [current, next, ..rest] -> {
      let new_row =
        list.window(current, 2)
        |> list.map(fn(pair) { list.fold(pair, 0, int.max) })
        |> list.zip(next)
        |> list.map(fn(pair) { pair.0 + pair.1 })

      max_path_sum_helper([new_row, ..rest])
    }
  }
}

pub fn main() {
  let assert Ok(input) = file.read(target) |> result.map(string.trim)
  let assert Ok(triangle) =
    string.split(input, "\n")
    |> list.try_map(fn(line) {
      string.split(line, " ") |> list.try_map(int.parse)
    })

  max_path_sum(triangle) |> io.debug
}
