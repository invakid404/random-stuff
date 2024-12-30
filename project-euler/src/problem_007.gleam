import gleam/io
import gleam/yielder
import lib/primes.{primes}

pub fn main() {
  let assert Ok(result) =
    primes()
    |> yielder.drop(10_000)
    |> yielder.first()

  io.debug(result)
}
