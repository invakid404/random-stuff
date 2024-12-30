import gleam/option.{type Option}

@external(erlang, "erlang", "put")
pub fn put(key: a, value: b) -> Nil

@external(erlang, "ffi", "get_maybe")
pub fn get(key: a) -> Option(b)
