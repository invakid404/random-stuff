import gleam/io
import gleam/option.{type Option, None, Some}
import glearray.{type Array}
import lib/dict

pub type Input {
  Input(up_to_coin: Int, target: Int)
}

fn insert_result(up_to_coin: Int, target: Int, result: Int) {
  dict.put(Input(up_to_coin: up_to_coin, target: target), result)
}

fn lookup_result(up_to_coin: Int, target: Int) -> Option(Int) {
  dict.get(Input(up_to_coin: up_to_coin, target: target))
}

fn recurse(coins: Array(Int), up_to_coin: Int, target: Int) {
  case up_to_coin, target {
    // out of bounds
    up_to_coin, _ if up_to_coin < 0 -> 0
    _, target if target < 0 -> 0
    // base cases
    _, target if target <= 1 -> 1
    0, _ -> 1
    // else
    _, _ ->
      case lookup_result(up_to_coin, target) {
        Some(result) -> result
        None -> {
          let assert Ok(coin) = glearray.get(coins, up_to_coin)

          let result =
            recurse(coins, up_to_coin - 1, target)
            + recurse(coins, up_to_coin, target - coin)

          insert_result(up_to_coin, target, result)

          result
        }
      }
  }
}

const target = 200

pub fn main() {
  let coins = [1, 2, 5, 10, 20, 50, 100, 200] |> glearray.from_list

  let result = recurse(coins, glearray.length(coins) - 1, target)
  io.debug(result)
}
