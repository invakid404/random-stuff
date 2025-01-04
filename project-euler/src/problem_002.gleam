import gleam/io
import gleam/yielder
import lib/yielder_utils.{fibonacci, sum, up_to}

pub fn main() {
  fibonacci()
  |> yielder.filter(fn(value) { value % 2 == 0 })
  |> up_to(4_000_000)
  |> sum
  |> io.debug
}
