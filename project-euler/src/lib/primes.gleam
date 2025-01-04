import gleam/erlang/atom
import gleam/float
import gleam/int
import gleam/option
import gleam/result
import gleam/yielder
import lib/dict

pub fn primes() {
  let assert Ok(primes) = primes_from(2)

  primes
}

pub fn primes_from(from: Int) {
  case from {
    _ if from < 2 -> Error(Nil)
    _ -> {
      let nums = yielder.unfold(from, fn(acc) { yielder.Next(acc, acc + 1) })

      nums
      |> yielder.filter(fn(n) {
        let assert Ok(sqrt_n) = int.square_root(n) |> result.map(float.truncate)

        nums
        |> yielder.take_while(fn(p) { p <= sqrt_n })
        |> yielder.all(fn(p) { n % p != 0 })
      })
      |> Ok
    }
  }
}

const up_to_atom_str = "primes_processed_up_to"

type NumberIsPrime {
  NumberIsPrime(n: Int)
}

fn memoize_primes(up_to: Int) {
  let up_to_atom =
    atom.from_string(up_to_atom_str)
    |> result.lazy_unwrap(fn() { atom.create_from_string(up_to_atom_str) })

  let already_processed = {
    dict.get(up_to_atom) |> option.unwrap(1)
  }

  case already_processed < up_to {
    True -> {
      let assert Ok(primes) = primes_from(already_processed + 1)
      let primes = primes |> yielder.take_while(fn(p) { p <= up_to })

      primes
      |> yielder.each(fn(p) { dict.put(NumberIsPrime(p), True) })

      dict.put(up_to_atom, up_to)
    }
    False -> Nil
  }
}

pub fn is_prime(n: Int) {
  memoize_primes(n)

  dict.get(NumberIsPrime(n)) |> option.is_some
}

pub fn prime_factors(n: Int) {
  yielder.unfold(#(n, 1, 0), fn(curr) {
    let #(n, factor, _amount) = curr
    case n {
      0 -> yielder.Done
      1 -> yielder.Next(curr, #(0, 0, 0))
      _ -> {
        let assert Ok(factor) =
          yielder.unfold(factor + 1, fn(f) { yielder.Next(f, f + 1) })
          |> yielder.find(fn(f) { n % f == 0 })

        let assert Ok(result) =
          yielder.unfold(n, fn(n) {
            case n {
              0 -> yielder.Done
              _ ->
                case n % factor {
                  0 -> yielder.Next(n, n / factor)
                  _ -> yielder.Next(n, 0)
                }
            }
          })
          |> yielder.index
          |> yielder.last
        let #(next_n, amount) = result

        yielder.Next(curr, #(next_n, factor, amount))
      }
    }
  })
  |> yielder.drop(1)
  |> yielder.map(fn(curr) { #(curr.1, curr.2) })
}
