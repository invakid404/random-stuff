import gleam/io
import gleam/yielder
import lib/yielder_utils.{sum, up_to}

fn fib() {
  #(0, 1)
  |> yielder.unfold(fn(curr) { yielder.Next(curr, #(curr.1, curr.0 + curr.1)) })
  |> yielder.map(fn(curr) { curr.0 })
}

pub fn main() {
  fib()
  |> yielder.filter(fn(value) { value % 2 == 0 })
  |> up_to(4_000_000)
  |> sum
  |> io.debug
}
