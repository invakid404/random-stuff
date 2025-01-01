import gleam/io
import gleam/result
import lib/math

const size = 20

pub fn main() {
  math.choose(size * 2, size) |> result.map(io.debug)
}
