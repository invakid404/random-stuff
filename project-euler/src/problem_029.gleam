import gleam/io
import gleam/option.{Some}
import gleam/yielder
import lib/dict
import lib/math

const limit = 100

pub fn main() {
  yielder.range(2, limit)
  |> yielder.flat_map(fn(a) {
    yielder.range(2, limit)
    |> yielder.map(fn(b) { math.pow(a, b) })
  })
  |> yielder.fold(0, fn(acc, curr) {
    case dict.get(curr) {
      Some(_) -> acc
      _ -> {
        dict.put(curr, True)
        acc + 1
      }
    }
  })
  |> io.debug
}
