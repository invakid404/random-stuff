import gleam/list
import gleam/result
import gleam/yielder
import lib/math.{factorial}

pub fn nth_permutation(input: List(a), n: Int) {
  let len = list.length(input)
  use total_permutations <- result.try(factorial(len))

  case n <= total_permutations {
    True -> nth_permutation_helper(input, n - 1, len, [])
    False -> {
      Error(Nil)
    }
  }
}

fn nth_permutation_helper(input: List(a), n: Int, len: Int, result: List(a)) {
  case len {
    0 -> result |> list.reverse |> Ok
    _ -> {
      use f <- result.try(factorial(len - 1))
      let pos = n / f
      let n = n % f

      let #(left, right) = list.split(input, pos)
      case right {
        [curr, ..right] ->
          nth_permutation_helper(list.append(left, right), n, len - 1, [
            curr,
            ..result
          ])
        _ -> Error(Nil)
      }
    }
  }
}

pub fn rotations(input: List(a)) {
  let n = list.length(input)

  yielder.iterate(input, fn(curr) {
    let assert Ok(next) = rotate_right(curr)

    next
  })
  |> yielder.take(n)
}

pub fn rotate_right(input: List(a)) {
  do_rotate_right(input, [])
}

fn do_rotate_right(input: List(a), acc: List(a)) {
  case input {
    [] -> Error(Nil)
    [x] -> Ok([x, ..list.reverse(acc)])
    [x, ..xs] -> do_rotate_right(xs, [x, ..acc])
  }
}
