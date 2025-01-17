import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
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

type MergeState {
  MergeState(
    left: Yielder(Int),
    right: Yielder(Int),
    left_curr: Option(Int),
    right_curr: Option(Int),
  )
}

pub fn merge_non_decreasing(left: Yielder(Int), right: Yielder(Int)) {
  let maybe_compare = fn(a: Option(Int), b: Option(Int)) {
    case a, b {
      Some(_), None -> True
      None, Some(_) -> False
      Some(a), Some(b) -> a <= b
      _, _ -> panic as "unreachable"
    }
  }

  let next = fn(in: Yielder(a)) {
    case yielder.step(in) {
      yielder.Next(in_head, rest) -> #(Some(in_head), rest)
      _ -> #(None, in)
    }
  }

  case yielder.step(left), yielder.step(right) {
    yielder.Next(left_first, left), yielder.Next(right_first, right) ->
      yielder.unfold(
        MergeState(left, right, Some(left_first), Some(right_first)),
        fn(acc) {
          case acc.left_curr, acc.right_curr {
            None, None -> yielder.Done
            _, _ ->
              case maybe_compare(acc.left_curr, acc.right_curr) {
                True -> {
                  let assert Some(left_curr) = acc.left_curr
                  let #(left_next, left) = next(acc.left)
                  yielder.Next(
                    left_curr,
                    MergeState(left, acc.right, left_next, acc.right_curr),
                  )
                }
                False -> {
                  let assert Some(right_curr) = acc.right_curr
                  let #(right_next, right) = next(acc.right)
                  yielder.Next(
                    right_curr,
                    MergeState(acc.left, right, acc.left_curr, right_next),
                  )
                }
              }
          }
        },
      )
    yielder.Next(_, _), yielder.Done -> left
    yielder.Done, yielder.Next(_, _) -> right
    _, _ -> yielder.empty()
  }
}

type FilterRepeatingState(a) {
  FilterRepeatingState(curr: a, repetitions: Int)
}

pub fn filter_repeating(in: Yielder(a), n: Int) {
  case yielder.step(in) {
    yielder.Next(head, rest) ->
      yielder.transform(rest, FilterRepeatingState(head, 1), fn(acc, curr) {
        let #(next, next_n) = case curr == acc.curr {
          True -> #(acc.curr, acc.repetitions + 1)
          False -> #(curr, 1)
        }

        yielder.Next(
          case next_n == n {
            True -> Ok(next)
            False -> Error(Nil)
          },
          FilterRepeatingState(next, next_n),
        )
      })
      |> yielder.filter_map(fn(x) { x })
    _ -> yielder.empty()
  }
}
