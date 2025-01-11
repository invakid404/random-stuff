import gleam/float
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/result
import gleam/string
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

pub fn pow(n: Int, p: Int) {
  case p {
    0 -> 1
    _ -> {
      let half = pow(n, int.bitwise_shift_right(p, 1))
      let squared = half * half

      case p % 2 {
        0 -> squared
        _ -> squared * n
      }
    }
  }
}

pub opaque type Fraction {
  Fraction(numerator: Int, denominator: Int)
}

pub fn make_fraction(numerator: Int, denominator: Int) {
  Fraction(numerator, denominator)
}

pub fn simplify_fraction(input: Fraction) {
  let gcd = gcd(input.numerator, input.denominator)

  Fraction(input.numerator / gcd, input.denominator / gcd)
}

pub fn multiply_fractions(left: Fraction, right: Fraction) {
  Fraction(
    left.numerator * right.numerator,
    left.denominator * right.denominator,
  )
  |> simplify_fraction
}

pub fn numerator(input: Fraction) {
  input.numerator
}

pub fn denominator(input: Fraction) {
  input.denominator
}

pub fn is_pandigital(input: Int) {
  is_n_pandigital(input, 9)
}

pub fn is_n_pandigital(input: Int, n_digits: Int) {
  string.inspect(input) |> is_n_pandigital_str(n_digits)
}

const digits = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

pub fn is_pandigital_str(input: String) {
  is_n_pandigital_str(input, 9)
}

pub fn is_n_pandigital_str(input: String, n_digits: Int) {
  digits |> list.take(n_digits) |> list.all(string.contains(input, _))
}

pub fn is_triangle_number(n: Int) {
  let m = 8 * n + 1
  use sqrt_m <- result.try(int.square_root(m))
  let sqrt_m = float.truncate(sqrt_m)

  Ok(sqrt_m * sqrt_m == m)
}
