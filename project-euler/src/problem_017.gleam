import gleam/int
import gleam/io
import gleam/result
import gleam/string
import gleam/yielder

pub fn number_to_words(n: Int) -> Result(String, String) {
  case n {
    n if n < 0 -> Error("Number must be non-negative")
    n if n >= 1_000_000 -> Error("Number too large")
    0 -> Ok("zero")
    1 -> Ok("one")
    2 -> Ok("two")
    3 -> Ok("three")
    4 -> Ok("four")
    5 -> Ok("five")
    6 -> Ok("six")
    7 -> Ok("seven")
    8 -> Ok("eight")
    9 -> Ok("nine")
    10 -> Ok("ten")
    11 -> Ok("eleven")
    12 -> Ok("twelve")
    13 -> Ok("thirteen")
    14 -> Ok("fourteen")
    15 -> Ok("fifteen")
    16 -> Ok("sixteen")
    17 -> Ok("seventeen")
    18 -> Ok("eighteen")
    19 -> Ok("nineteen")
    20 -> Ok("twenty")
    30 -> Ok("thirty")
    40 -> Ok("forty")
    50 -> Ok("fifty")
    60 -> Ok("sixty")
    70 -> Ok("seventy")
    80 -> Ok("eighty")
    90 -> Ok("ninety")
    n if n > 20 && n < 100 -> {
      let tens = n / 10 * 10
      let ones = n % 10
      case ones {
        0 -> number_to_words(tens)
        _ ->
          case number_to_words(tens), number_to_words(ones) {
            Ok(tens_word), Ok(ones_word) -> Ok(tens_word <> "-" <> ones_word)
            Error(e), _ -> Error(e)
            _, Error(e) -> Error(e)
          }
      }
    }
    n if n >= 100 && n < 1000 -> {
      let hundreds = n / 100
      let remainder = n % 100
      case number_to_words(hundreds) {
        Error(e) -> Error(e)
        Ok(hundreds_word) ->
          case remainder {
            0 -> Ok(hundreds_word <> " hundred")
            _ ->
              case number_to_words(remainder) {
                Error(e) -> Error(e)
                Ok(remainder_word) ->
                  Ok(hundreds_word <> " hundred and " <> remainder_word)
              }
          }
      }
    }
    n if n >= 1000 && n < 1_000_000 -> {
      let thousands = n / 1000
      let remainder = n % 1000
      case number_to_words(thousands) {
        Error(e) -> Error(e)
        Ok(thousands_word) ->
          case remainder {
            0 -> Ok(thousands_word <> " thousand")
            _ ->
              case number_to_words(remainder) {
                Error(e) -> Error(e)
                Ok(remainder_word) ->
                  case remainder >= 100 {
                    True -> Ok(thousands_word <> " thousand " <> remainder_word)
                    False ->
                      Ok(thousands_word <> " thousand and " <> remainder_word)
                  }
              }
          }
      }
    }
    _ -> Error("Unexpected number")
  }
}

const target = 1000

pub fn main() {
  yielder.range(1, target)
  |> yielder.filter_map(number_to_words)
  |> yielder.map(string.replace(_, " ", ""))
  |> yielder.map(string.replace(_, "-", ""))
  |> yielder.map(string.length)
  |> yielder.reduce(int.add)
  |> result.map(io.debug)
}
