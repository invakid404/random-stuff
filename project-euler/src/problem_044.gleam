import gleam/float
import gleam/int
import gleam/io
import gleam/result
import gleam/yielder

fn is_pentagonal(n: Int) {
  let inner = 1 + 24 * n
  let assert Ok(inner_sqrt) =
    int.square_root(inner) |> result.map(float.truncate)

  inner_sqrt * inner_sqrt == inner && { 1 + inner_sqrt } % 6 == 0
}

fn pentagonal_numbers() {
  yielder.unfold(#(1, 4), fn(state) {
    let #(p, d) = state
    yielder.Next(p, #(p + d, d + 3))
  })
}

pub fn main() {
  pentagonal_numbers()
  |> yielder.find_map(fn(sum) {
    pentagonal_numbers()
    |> yielder.map(fn(lower) {
      let upper = sum - lower

      #(lower, upper)
    })
    |> yielder.take_while(fn(pair) {
      let #(lower, upper) = pair

      lower <= upper
    })
    |> yielder.find_map(fn(pair) {
      let #(lower, upper) = pair
      let diff = upper - lower

      case is_pentagonal(upper) && is_pentagonal(diff) {
        True -> Ok(diff)
        False -> Error(Nil)
      }
    })
  })
  |> result.map(io.debug)
}
