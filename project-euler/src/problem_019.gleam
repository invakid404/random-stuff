import gleam/io
import gleam/yielder

const start_year = 1901

const end_year = 2000

const initial_year = 1900

pub fn days_in_month(year: Int, month: Int) -> Int {
  case month {
    2 -> {
      case is_leap_year(year) {
        True -> 29
        False -> 28
      }
    }
    4 | 6 | 9 | 11 -> 30
    _ -> 31
  }
}

pub fn is_leap_year(year: Int) -> Bool {
  case True {
    _ if year % 400 == 0 -> True
    _ if year % 100 == 0 -> False
    _ if year % 4 == 0 -> True
    _ -> False
  }
}

fn process_month(
  year: Int,
  month: Int,
  day_of_week: Int,
  count: Int,
) -> #(Int, Int) {
  let new_count = case day_of_week {
    0 -> count + 1
    _ -> count
  }
  let new_day = { day_of_week + days_in_month(year, month) } % 7
  #(new_day, new_count)
}

fn process_year(year: Int, start_day: Int, count: Int) -> #(Int, Int) {
  yielder.range(1, 12)
  |> yielder.fold(#(start_day, count), fn(acc, month) {
    let #(day, cnt) = acc
    process_month(year, month, day, cnt)
  })
}

fn advance_loop(current_year: Int, day: Int) -> Int {
  case current_year >= start_year {
    True -> day
    False -> {
      let #(new_day, _) = process_year(current_year, day, 0)
      advance_loop(current_year + 1, new_day)
    }
  }
}

fn advance_to_start() -> Int {
  advance_loop(initial_year, 1)
}

fn count_sundays(start_year: Int, end_year: Int, day: Int, count: Int) -> Int {
  case start_year > end_year {
    True -> count
    False -> {
      let #(new_day, new_count) = process_year(start_year, day, count)
      count_sundays(start_year + 1, end_year, new_day, new_count)
    }
  }
}

pub fn main() {
  let start_day = advance_to_start()

  count_sundays(start_year, end_year, start_day, 0)
  |> io.debug()
}
