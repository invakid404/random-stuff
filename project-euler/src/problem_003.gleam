import gleam/io

const target = 600_851_475_143

fn solve(target: Int, factor: Int) {
  case target {
    1 -> factor
    _ ->
      case target % factor {
        0 -> solve(target / factor, factor)
        _ -> solve(target, factor + 1)
      }
  }
}

pub fn main() {
  solve(target, 2) |> io.debug
}
