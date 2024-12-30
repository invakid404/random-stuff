import gleam/io

fn sum(n: Int) {
  { n * { n + 1 } } / 2
}

fn sum_of_squares(n: Int) {
  { n * { n + 1 } * { 2 * n + 1 } } / 6
}

const target = 100

pub fn main() {
  let a = sum(target)
  let b = sum_of_squares(target)

  a * a - b |> io.debug
}
