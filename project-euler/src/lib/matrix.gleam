import gleam/list
import gleam/result
import gleam/yielder
import glearray.{type Array}

pub opaque type Matrix(a) {
  Matrix(data: Array(Array(a)), rows: Int, cols: Int)
}

pub fn from_list(data: List(List(a))) {
  let matrix = list.map(data, glearray.from_list) |> glearray.from_list

  let n_rows = glearray.length(matrix)
  let assert Ok(n_cols) = glearray.get(matrix, 0) |> result.map(glearray.length)

  // Assert all rows have the same amount of columns
  let assert True =
    yielder.range(0, n_rows - 1)
    |> yielder.map(glearray.get(matrix, _))
    |> yielder.all(fn(row) {
      case row {
        Ok(row) -> glearray.length(row) == n_cols
        _ -> False
      }
    })

  Matrix(data: matrix, rows: n_rows, cols: n_cols)
}

pub fn at(matrix: Matrix(a), row: Int, col: Int) {
  use row <- result.try(glearray.get(matrix.data, row))
  use val <- result.try(glearray.get(row, col))

  Ok(val)
}

pub fn rows(matrix: Matrix(a)) {
  matrix.rows
}

pub fn cols(matrix: Matrix(a)) {
  matrix.cols
}
