import gleam/option.{Some}
import gleam/result
import lib/dict

pub fn gcd(a: Int, b: Int) {
  case b {
    0 -> a
    _ -> gcd(b, a % b)
  }
}

pub fn lcm(a: Int, b: Int) {
  a / gcd(a, b) * b
}

type FactorialInput {
  FactorialInput(n: Int)
}

pub fn factorial(n: Int) {
  case n {
    n if n < 0 -> Error(Nil)
    n if n <= 1 -> Ok(1)
    _ -> {
      let input = FactorialInput(n)
      case dict.get(input) {
        Some(result) -> Ok(result)
        _ -> {
          use prev <- result.try(factorial(n - 1))

          let res = n * prev
          dict.put(input, res)

          Ok(res)
        }
      }
    }
  }
}

pub fn choose(n: Int, k: Int) {
  use a <- result.try(factorial(n))
  use b <- result.try(factorial(k))
  use c <- result.try(factorial(n - k))

  Ok(a / b / c)
}
