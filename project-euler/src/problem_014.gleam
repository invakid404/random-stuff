import gleam/io
import gleam/option.{Some}
import gleam/yielder
import lib/dict

fn collatz(n: Int) {
  case n {
    n if n <= 1 -> n
    _ ->
      case dict.get(n) {
        Some(result) -> result
        _ -> {
          let next = case n % 2 {
            0 -> n / 2
            _ -> 3 * n + 1
          }

          let result = collatz(next) + 1
          dict.put(n, result)

          result
        }
      }
  }
}

const target = 1_000_000

pub fn main() {
  let assert Ok(result) =
    yielder.range(1, target - 1)
    |> yielder.map(fn(n) { #(n, collatz(n)) })
    |> yielder.reduce(fn(acc, curr) {
      case acc.1 < curr.1 {
        True -> curr
        False -> acc
      }
    })

  result.0 |> io.debug
}
