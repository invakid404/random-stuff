-module(ffi).

-export([get_maybe/1]).

get_maybe(Key) ->
    case get(Key) of
        undefined -> none;
        Value -> {some, Value}
    end.
