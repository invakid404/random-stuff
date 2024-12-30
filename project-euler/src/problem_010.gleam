import gleam/io
import gleam/list
import gleam/yielder
import lib/primes.{primes}

const target = 2_000_000

pub fn main() {
  primes()
  |> yielder.fold_until(0, fn(acc, val) {
    case val {
      val if val < target -> list.Continue(acc + val)
      _ -> list.Stop(acc)
    }
  })
  |> io.debug()
}
