import gleam/io
import gleam/result
import gleam/yielder
import lib/math.{pow}
import lib/primes.{prime_factors}

fn cycle_length(n: Int) {
  let n =
    prime_factors(n)
    |> yielder.filter(fn(factor) { factor.0 == 2 || factor.0 == 5 })
    |> yielder.fold(n, fn(acc, factor) { acc / { pow(factor.0, factor.1) } })

  case n {
    1 -> 0
    _ -> {
      1
      + {
        yielder.unfold(10, fn(k) { yielder.Next(k, k * 10) })
        |> yielder.take_while(fn(k) { k % n != 1 })
        |> yielder.length
      }
    }
  }
}

const target = 1000

pub fn main() {
  yielder.range(2, target)
  |> yielder.map(fn(n) { #(n, cycle_length(n)) })
  |> yielder.reduce(fn(max, curr) {
    case max.1 < curr.1 {
      True -> curr
      False -> max
    }
  })
  |> result.map(fn(result) { result.0 })
  |> result.map(io.debug)
}
