import gleam/io
import lib/yielder_utils.{multiples_of, skip_every, sum, up_to}

const target = 1000

pub fn main() {
  let multiples_of_3 = multiples_of(3) |> up_to(target) |> sum
  let multiples_of_5 = multiples_of(5) |> up_to(target) |> skip_every(3) |> sum

  io.debug(multiples_of_3 + multiples_of_5)
}
