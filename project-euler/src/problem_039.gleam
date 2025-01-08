import gleam/io
import gleam/result
import gleam/yielder

const upper_limit = 1000

pub fn main() {
  yielder.range(3, upper_limit)
  |> yielder.map(fn(p) {
    let max_a = p / 2
    let results =
      yielder.range(1, max_a)
      |> yielder.flat_map(fn(a) {
        let max_b = p - a
        yielder.range(a, max_b)
        |> yielder.filter_map(fn(b) {
          let c = p - { a + b }
          case c > b && a * a + b * b == c * c {
            True -> Ok(#(a, b, c))
            False -> Error(Nil)
          }
        })
      })

    #(p, results |> yielder.length)
  })
  |> yielder.reduce(fn(acc, curr) {
    case acc.1 < curr.1 {
      True -> curr
      False -> acc
    }
  })
  |> result.map(io.debug)
}
