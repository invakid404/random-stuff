import gleam/float
import gleam/int
import gleam/list
import gleam/result
import gleam/yielder.{type Yielder}

pub fn infinite_range(from: Int, step: Int) {
  yielder.unfold(from, fn(acc) { yielder.Next(acc, acc + step) })
}

pub fn multiples_of(n: Int) {
  infinite_range(n, n)
}

pub fn up_to(in: Yielder(Int), target: Int) {
  yielder.take_while(in, fn(value) { value < target })
}

pub fn skip_every(in: Yielder(a), n: Int) {
  in
  |> yielder.sized_chunk(n)
  |> yielder.flat_map(fn(chunk) {
    chunk |> list.take(n - 1) |> yielder.from_list
  })
}

pub fn sum(in: Yielder(Int)) {
  yielder.fold(in, 0, fn(acc, value) { acc + value })
}

fn divisors_base(n: Int) {
  use sqrt_n <- result.try(int.square_root(n))
  let sqrt_n = float.truncate(sqrt_n)

  Ok(
    yielder.range(1, sqrt_n)
    |> yielder.filter(fn(x) { n % x == 0 }),
  )
}

pub fn divisors(n: Int) {
  use base <- result.try(divisors_base(n))

  Ok(
    base
    |> yielder.flat_map(fn(divisor) {
      let other = n / divisor

      case divisor == other {
        True -> [divisor]
        False -> [divisor, other]
      }
      |> yielder.from_list
    }),
  )
}

pub fn proper_divisors(n: Int) {
  use divisors <- result.try(divisors(n))

  divisors |> yielder.filter(fn(d) { n != d }) |> Ok
}

pub fn proper_divisors_sum(n: Int) {
  use proper_divisors <- result.try(proper_divisors(n))

  proper_divisors |> yielder.reduce(int.add)
}

pub fn n_divisors(n: Int) {
  use base <- result.try(divisors_base(n))

  Ok(
    base
    |> yielder.fold(0, fn(acc, divisor) {
      let other = n / divisor

      acc
      + case divisor == other {
        True -> 1
        False -> 2
      }
    }),
  )
}

pub fn fibonacci() {
  #(0, 1)
  |> yielder.unfold(fn(curr) { yielder.Next(curr, #(curr.1, curr.0 + curr.1)) })
  |> yielder.map(fn(curr) { curr.0 })
}

pub fn permutations(list: List(a)) -> Yielder(List(a)) {
  case list {
    [] -> yielder.single([])
    _ -> {
      let list = yielder.from_list(list) |> yielder.index

      list
      |> yielder.flat_map(fn(i_pair) {
        let #(i, i_idx) = i_pair

        list
        |> yielder.fold([], fn(acc, j_pair) {
          let #(j, j_idx) = j_pair
          case i_idx == j_idx {
            True -> acc
            False -> [j, ..acc]
          }
        })
        |> list.reverse
        |> permutations
        |> yielder.map(fn(permutation) { [i, ..permutation] })
      })
    }
  }
}
