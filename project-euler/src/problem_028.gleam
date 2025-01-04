import gleam/io
import lib/math

const size = 1001

pub fn main() {
  // I totally didn't look the formula up
  {
    { 2 * { math.pow(size, 3) - size } / 3 }
    + { { math.pow(size, 2) - 1 } / 2 }
    + { 2 * size }
    - 1
  }
  |> io.debug
}
