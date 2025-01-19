import gleam/io
import gleam/list
import gleam/result
import gleam/yielder
import lib/math
import lib/result_utils
import lib/yielder_utils

const window_size = 4

pub fn main() {
  let assert Ok(windows) =
    yielder_utils.infinite_range(644, 1)
    |> yielder.map(fn(n) {
      #(
        n,
        math.prime_factors(n) |> yielder.map(fn(x) { x.0 }) |> yielder.to_list,
      )
    })
    |> yielder.filter(fn(curr) { list.length(curr.1) == window_size })
    |> yielder_utils.window(window_size)

  windows
  |> yielder.find(fn(window) {
    let assert Ok(curr) =
      window
      |> yielder.from_list
      |> yielder.map(fn(pair) { pair.0 })
      |> yielder_utils.window(2)

    curr
    |> yielder.all(fn(pair) {
      let assert [a, b] = pair
      b - a == 1
    })
  })
  |> result_utils.flat_map(list.first)
  |> result.map(fn(res) { res.0 })
  |> result.map(io.debug)
}
