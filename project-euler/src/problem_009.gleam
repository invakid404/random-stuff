import gleam/io
import gleam/list
import gleam/result
import gleam/yielder

const target = 1000

pub fn main() {
  let assert Ok(result) =
    yielder.range(target, 1)
    |> yielder.find_map(fn(c) {
      yielder.range(c - 1, 1)
      |> yielder.find_map(fn(b) {
        let a = target - c - b

        case a, b, c {
          a, b, c if 0 < a && a < b && a * a + b * b == c * c -> Ok([a, b, c])
          _, _, _ -> Error(Nil)
        }
      })
    })
    |> result.map(fn(res) { list.fold(res, 1, fn(acc, value) { acc * value }) })

  io.debug(result)
}
