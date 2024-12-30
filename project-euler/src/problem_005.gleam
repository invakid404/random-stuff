import gleam/io
import gleam/yielder
import lib/math.{lcm}

const target = 20

pub fn main() {
  let assert Ok(result) =
    yielder.range(2, target)
    |> yielder.reduce(fn(acc, value) { lcm(acc, value) })

  io.debug(result)
}
