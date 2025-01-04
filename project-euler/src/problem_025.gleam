import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/yielder
import lib/yielder_utils.{fibonacci}

pub fn main() {
  fibonacci()
  |> yielder.index
  |> yielder.find_map(fn(pair) {
    let #(n, index) = pair
    use digits <- result.try(int.digits(n, 10))

    case list.length(digits) >= 1000 {
      True -> Ok(index)
      False -> Error(Nil)
    }
  })
  |> result.map(io.debug)
}
