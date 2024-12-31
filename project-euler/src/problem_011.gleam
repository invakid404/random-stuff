import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import gleam/yielder.{type Yielder}
import lib/file
import lib/matrix

const input_filename = "src/problem_011.txt"

const window_size = 4

type Position =
  List(Int)

type Window =
  List(Position)

pub fn main() {
  let assert Ok(input) = file.read(input_filename) |> result.map(string.trim)
  let assert Ok(input) =
    string.split(input, "\n")
    |> list.try_map(fn(line) {
      string.split(line, " ") |> list.try_map(int.parse)
    })
    |> result.map(matrix.from_list)

  let rows = matrix.rows(input)
  let cols = matrix.cols(input)

  let assert True = rows == cols

  let size = rows

  let find_max = fn(windows: Yielder(Window)) {
    let assert Ok(result) =
      windows
      |> yielder.filter_map(fn(window) {
        list.try_map(window, fn(pos) {
          case pos {
            [row, col] -> matrix.at(input, row, col)
            _ -> Error(Nil)
          }
        })
      })
      |> yielder.map(list.fold(_, 1, fn(acc, val) { acc * val }))
      |> yielder.reduce(fn(acc, val) { int.max(acc, val) })

    result
  }

  let horizontal_windows = {
    let col_windows =
      yielder.range(0, size - 1) |> yielder.to_list |> list.window(window_size)

    yielder.range(0, size - 1)
    |> yielder.flat_map(fn(row) {
      list.map(col_windows, list.map(_, fn(col) { [row, col] }))
      |> yielder.from_list
    })
  }

  let vertical_windows =
    horizontal_windows |> yielder.map(list.map(_, list.reverse))

  let diagonal_a_windows = {
    let max_size = size - window_size

    let start_positions =
      yielder.range(0, max_size)
      |> yielder.flat_map(fn(row) {
        yielder.range(0, max_size) |> yielder.map(fn(col) { #(row, col) })
      })

    start_positions
    |> yielder.map(fn(pos) {
      yielder.range(0, window_size - 1)
      |> yielder.map(fn(offset) { [pos.0 + offset, pos.1 + offset] })
      |> yielder.to_list
    })
  }

  let diagonal_b_windows =
    diagonal_a_windows
    |> yielder.map(fn(window) {
      window
      |> list.transpose
      |> fn(v) {
        case v {
          [rows, cols] -> [rows, list.reverse(cols)]
          _ -> panic
        }
      }
      |> list.transpose
    })

  let assert Ok(result) =
    [
      find_max(horizontal_windows),
      find_max(vertical_windows),
      find_max(diagonal_a_windows),
      find_max(diagonal_b_windows),
    ]
    |> list.reduce(fn(acc, val) { int.max(acc, val) })
  io.debug(result)
}
