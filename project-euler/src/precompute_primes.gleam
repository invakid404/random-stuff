import bravo
import bravo/uset
import file_streams/file_stream
import gleam/dict
import gleam/dynamic
import gleam/erlang
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/otp/task
import gleam/result
import gleam/string
import gleam/yielder
import lib/primes

const target = 1_000_000_000

const batches = 12

pub type PrimeTable =
  uset.USet(Int, Nil)

pub type PrimeDict =
  dict.Dict(Int, Nil)

fn make_table_name() {
  "precomputed_primes_" <> string.inspect(target)
}

fn make_output_name() {
  "data/" <> make_table_name() <> ".bin"
}

pub fn load_primes() {
  load_primes_from_file(make_output_name())
}

pub fn load_primes_from_file(path: String) -> Result(PrimeDict, dynamic.Dynamic) {
  use in <- result.try(
    file_stream.open_read(path) |> result.map_error(dynamic.from),
  )
  use bytes <- result.try(
    file_stream.read_remaining_bytes(in) |> result.map_error(dynamic.from),
  )

  use map <- result.try(
    erlang.unsafe_binary_to_term(bytes) |> result.map_error(dynamic.from),
  )
  let parser = dynamic.dict(dynamic.int, fn(_) { Ok(Nil) })

  parser(map) |> result.map_error(dynamic.from)
}

pub fn yielder_from_table(
  prime_table: PrimeTable,
) -> Result(yielder.Yielder(Int), bravo.BravoError) {
  use key <- result.try(uset.first(prime_table))

  yielder.unfold(Some(key), fn(curr) {
    case curr {
      Some(key) ->
        yielder.Next(key, case uset.next(prime_table, key) {
          Ok(next) -> Some(next)
          _ -> None
        })
      _ -> yielder.Done
    }
  })
  |> Ok
}

pub fn is_prime(prime_table: PrimeDict, n: Int) {
  dict.has_key(prime_table, n)
}

pub fn main() {
  use table <- result.try(uset.new(make_table_name(), bravo.Public))

  let target = target + 1

  let ranges = {
    yielder.range(0, batches - 1)
    |> yielder.map(fn(i) {
      let target_pow = {
        let assert Ok(p) = float.power(int.to_float(target), 1.5)
        p
      }
      let chunk_work = target_pow /. int.to_float(batches)

      let end = {
        let work_done = chunk_work *. int.to_float(batches - i)
        let assert Ok(p) = float.power(work_done, 2.0 /. 3.0)
        float.truncate(p)
      }
      let start = {
        let work_done = chunk_work *. int.to_float(batches - i - 1)
        let assert Ok(p) = float.power(work_done, 2.0 /. 3.0)
        float.truncate(p)
      }

      #(int.max(2, start), end, i + 1)
    })
  }

  ranges
  |> yielder.to_list
  |> list.map(fn(range) {
    io.println("Starting task for range " <> string.inspect(range) <> ".")
    task.async(fn() {
      let start_time = erlang.system_time(erlang.Millisecond)
      let #(min, max, idx) = range
      let assert Ok(primes) = primes.primes_from(min)
      let chunk_size = int.max(1000, { max - min } / 100)

      primes
      |> yielder.take_while(fn(p) { p <= max })
      |> yielder.sized_chunk(chunk_size)
      |> yielder.each(fn(chunk) {
        uset.insert_list(table, chunk |> list.map(fn(p) { #(p, Nil) }))
      })

      let end_time = erlang.system_time(erlang.Millisecond)
      let duration = end_time - start_time
      io.println(
        "Task "
        <> string.inspect(idx)
        <> " completed in "
        <> string.inspect(duration)
        <> "ms.",
      )
    })
  })
  |> list.each(task.await_forever)

  let start_time = erlang.system_time(erlang.Millisecond)
  use primes_yielder <- result.try(yielder_from_table(table))
  let primes_map =
    primes_yielder
    |> yielder.fold(dict.new(), fn(acc, prime) { dict.insert(acc, prime, Nil) })

  let primes_map = erlang.term_to_binary(primes_map)
  let assert Ok(out_file) = file_stream.open_write(make_output_name())
  let assert Ok(_) = file_stream.write_bytes(out_file, primes_map)
  let assert Ok(_) = file_stream.close(out_file)
  let end_time = erlang.system_time(erlang.Millisecond)

  let duration = end_time - start_time
  io.println(
    "Converting ETS to dict completed in " <> string.inspect(duration) <> "ms.",
  )

  Ok(Nil)
}
