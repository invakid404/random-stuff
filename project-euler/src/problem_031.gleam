import bravo
import bravo/uset
import gleam/io
import gleam/result
import glearray.{type Array}

pub type Input {
  Input(up_to_coin: Int, target: Int)
}

type State =
  #(Input, Int)

type Table =
  uset.USet(State)

fn make_table() {
  let assert Ok(table) = uset.new("problem_31", 1, bravo.Private)

  table
}

fn insert_result(table: Table, up_to_coin: Int, target: Int, result: Int) {
  uset.insert(table, [#(Input(up_to_coin: up_to_coin, target: target), result)])
}

fn lookup_result(table: Table, up_to_coin: Int, target: Int) {
  uset.lookup(table, Input(up_to_coin: up_to_coin, target: target))
  |> result.map(fn(result) { result.1 })
}

fn recurse(coins: Array(Int), table: Table, up_to_coin: Int, target: Int) {
  case up_to_coin, target {
    // out of bounds
    up_to_coin, _ if up_to_coin < 0 -> 0
    _, target if target < 0 -> 0
    // base cases
    _, target if target <= 1 -> 1
    0, _ -> 1
    // else
    _, _ ->
      case lookup_result(table, up_to_coin, target) {
        Ok(result) -> result
        _ -> {
          let assert Ok(coin) = glearray.get(coins, up_to_coin)

          let result =
            recurse(coins, table, up_to_coin - 1, target)
            + recurse(coins, table, up_to_coin, target - coin)

          insert_result(table, up_to_coin, target, result)

          result
        }
      }
  }
}

const target = 200

pub fn main() {
  let coins = [1, 2, 5, 10, 20, 50, 100, 200] |> glearray.from_list
  let table = make_table()

  let result = recurse(coins, table, glearray.length(coins) - 1, target)
  io.debug(result)
}
