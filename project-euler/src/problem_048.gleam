import gleam/io
import gleam/result
import gleam/yielder
import lib/math

const target = 1000

const digits = 10

pub fn main() {
  let mod = math.pow(10, digits)

  yielder.range(1, target)
  |> yielder.map(fn(i) { math.modpow(i, i, mod) })
  |> yielder.reduce(fn(acc, curr) { { acc + curr } % mod })
  |> result.map(io.debug)
}
