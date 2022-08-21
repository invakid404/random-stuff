/*
  151 - Query String Parser
  -------
  by Pig Fang (@g-plane) #extreme #template-literal
  
  ### Question
  
  You're required to implement a type-level parser to parse URL query string into a object literal type.
  
  Some detailed requirements:
  
  - Value of a key in query string can be ignored but still be parsed to `true`. For example, `'key'` is without value, so the parser result is `{ key: true }`.
  - Duplicated keys must be merged into one. If there are different values with the same key, values must be merged into a tuple type.
  - When a key has only one value, that value can't be wrapped into a tuple type.
  - If values with the same key appear more than once, it must be treated as once. For example, `key=value&key=value` must be treated as `key=value` only.
  
  > View on GitHub: https://tsch.js.org/151
*/

/* _____________ Your Code Here _____________ */
type Split<Value extends string, Delimiter extends string> = Value extends ''
  ? []
  : Value extends `${infer Prefix}${Delimiter}${infer Suffix}`
  ? [Prefix, ...Split<Suffix, Delimiter>]
  : [Value];

type ParseQueryString<Query extends string> = GroupEntries<
  ParseQueryParams<Query>
>;

type ParseQueryParams<Query extends string> = Split<
  Query,
  '&'
> extends infer Params extends readonly string[]
  ? { [key in keyof Params]: ParseQueryParam<Params[key]> }
  : never;

type ParseQueryParam<Param extends string> = Split<
  Param,
  '='
> extends infer ParsedParam
  ? ParsedParam extends [infer Name]
    ? [Name, true]
    : ParsedParam
  : never;

type Unique<Value> = Value extends readonly any[]
  ? UnwrapTuple<UniqueTuple<Value>>
  : Value;

type UniqueTuple<Tuple extends readonly any[]> = Tuple extends []
  ? []
  : Tuple extends [infer Head, ...infer Rest]
  ? Head extends Rest[number]
    ? Unique<Rest>
    : [Head, ...UniqueTuple<Rest>]
  : never;

type GroupEntries<
  Entries extends readonly any[],
  Accumulator = {},
> = Entries extends []
  ? { [key in keyof Accumulator]: Unique<Accumulator[key]> }
  : Entries extends [infer Entry, ...infer Rest]
  ? Entry extends [infer Key extends string, infer Value]
    ? Key extends keyof Accumulator
      ? GroupEntries<
          Rest,
          Omit<Accumulator, Key> & {
            [key in Key]: [...ToTuple<Accumulator[key]>, Value];
          }
        >
      : GroupEntries<Rest, Accumulator & { [key in Key]: Value }>
    : never
  : never;

type ToTuple<Value> = Value extends readonly any[] ? Value : [Value];

type UnwrapTuple<Value> = Value extends [infer Value] ? Value : Value;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<ParseQueryString<''>, {}>>,
  Expect<Equal<ParseQueryString<'k1'>, { k1: true }>>,
  Expect<Equal<ParseQueryString<'k1&k1'>, { k1: true }>>,
  Expect<Equal<ParseQueryString<'k1&k2'>, { k1: true; k2: true }>>,
  Expect<Equal<ParseQueryString<'k1=v1'>, { k1: 'v1' }>>,
  Expect<Equal<ParseQueryString<'k1=v1&k1=v2'>, { k1: ['v1', 'v2'] }>>,
  Expect<Equal<ParseQueryString<'k1=v1&k2=v2'>, { k1: 'v1'; k2: 'v2' }>>,
  Expect<
    Equal<ParseQueryString<'k1=v1&k2=v2&k1=v2'>, { k1: ['v1', 'v2']; k2: 'v2' }>
  >,
  Expect<Equal<ParseQueryString<'k1=v1&k2'>, { k1: 'v1'; k2: true }>>,
  Expect<Equal<ParseQueryString<'k1=v1&k1=v1'>, { k1: 'v1' }>>,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/151/answer
  > View solutions: https://tsch.js.org/151/solutions
  > More Challenges: https://tsch.js.org
*/
