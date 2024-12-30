import gleam/list
import gleam/yielder.{type Yielder}

pub fn multiples_of(n: Int) {
  yielder.unfold(n, fn(acc) { yielder.Next(acc, acc + n) })
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
