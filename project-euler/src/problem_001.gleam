import gleam/io
import gleam/list
import gleam/yielder.{type Yielder}

fn multiples_of(n: Int) {
  yielder.unfold(n, fn(acc) { yielder.Next(acc, acc + n) })
}

fn up_to(in: Yielder(Int), target: Int) {
  yielder.take_while(in, fn(value) { value < target })
}

fn skip_every(in: Yielder(a), n: Int) {
  in
  |> yielder.sized_chunk(n)
  |> yielder.flat_map(fn(chunk) {
    chunk |> list.take(n - 1) |> yielder.from_list
  })
}

fn sum(in: Yielder(Int)) {
  yielder.fold(in, 0, fn(acc, value) { acc + value })
}

const target = 1000

pub fn main() {
  let multiples_of_3 = multiples_of(3) |> up_to(target) |> sum
  let multiples_of_5 = multiples_of(5) |> up_to(target) |> skip_every(3) |> sum

  io.debug(multiples_of_3 + multiples_of_5)
}
