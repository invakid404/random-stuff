import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import gleam/yielder
import lib/dict
import lib/primes
import lib/yielder_utils

pub fn main() {
  let assert Ok(primes) = primes.primes_from(1000)
  let primes =
    primes
    |> yielder.take_while(fn(p) { p <= 10_000 })
    |> yielder.to_list

  // Memoize
  primes
  |> list.each(fn(p) { dict.put(p, True) })

  primes
  |> yielder.from_list
  |> yielder.flat_map(fn(p) {
    let assert Ok(digits) = int.digits(p, 10)

    let prime_permutations =
      digits
      |> yielder_utils.permutations
      |> yielder.filter_map(int.undigits(_, 10))
      |> yielder.filter(fn(curr) { dict.get(curr) |> option.is_some })
      |> yielder.filter(fn(curr) { curr >= p })
      |> yielder.to_list
      |> list.sort(int.compare)
      |> list.unique

    let sequences =
      prime_permutations
      |> yielder.iterate(list.drop(_, 1))
      |> yielder.take_while(fn(curr) { list.length(curr) >= 2 })
      |> yielder.filter_map(fn(curr) {
        let assert [second, ..rest] = curr
        let delta = second - p

        rest
        |> list.find(fn(third) { third - second == delta })
        |> result.map(fn(third) { [p, second, third] })
      })

    sequences
  })
  |> yielder.last
  |> result.map(list.fold(_, "", fn(acc, curr) { acc <> string.inspect(curr) }))
  |> result.map(io.println)
}
