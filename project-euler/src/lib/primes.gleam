import gleam/float
import gleam/int
import gleam/result
import gleam/yielder

pub fn primes() {
  let nums = yielder.unfold(2, fn(acc) { yielder.Next(acc, acc + 1) })

  nums
  |> yielder.filter(fn(n) {
    let assert Ok(sqrt_n) = int.square_root(n) |> result.map(float.truncate)

    nums
    |> yielder.take_while(fn(p) { p <= sqrt_n })
    |> yielder.all(fn(p) { n % p != 0 })
  })
}
