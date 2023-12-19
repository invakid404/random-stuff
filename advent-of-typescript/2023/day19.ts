type Items = {
  0: "🛹";
  1: "🚲";
  2: "🛴";
  3: "🏄";
};

type GetItem<T extends readonly unknown[]> = T["length"] extends keyof Items
  ? Items[T["length"]]
  : never;

type NextItemIndex<N extends readonly unknown[]> = N["length"] extends 3
  ? []
  : [...N, unknown];

type Repeat<
  T,
  N extends number,
  Acc extends readonly unknown[] = [],
> = Acc["length"] extends N ? Acc : Repeat<T, N, [...Acc, T]>;

type Rebuild<
  T extends readonly number[],
  Acc extends readonly string[] = [],
  Index extends readonly unknown[] = [],
> = T extends [
  infer Head extends number,
  ...infer Rest extends readonly number[],
]
  ? Rebuild<
      Rest,
      Repeat<GetItem<Index>, Head> extends infer R extends readonly string[]
        ? [...Acc, ...R]
        : Acc,
      NextItemIndex<Index>
    >
  : Acc;

import { Expect, Equal } from "type-testing";

type test_0_actual = Rebuild<[2, 1, 3, 3, 1, 1, 2]>;
//   ^?
type test_0_expected = [
  "🛹",
  "🛹",
  "🚲",
  "🛴",
  "🛴",
  "🛴",
  "🏄",
  "🏄",
  "🏄",
  "🛹",
  "🚲",
  "🛴",
  "🛴",
];
type test_0 = Expect<Equal<test_0_expected, test_0_actual>>;

type test_1_actual = Rebuild<[3, 3, 2, 1, 2, 1, 2]>;
//   ^?
type test_1_expected = [
  "🛹",
  "🛹",
  "🛹",
  "🚲",
  "🚲",
  "🚲",
  "🛴",
  "🛴",
  "🏄",
  "🛹",
  "🛹",
  "🚲",
  "🛴",
  "🛴",
];
type test_1 = Expect<Equal<test_1_expected, test_1_actual>>;

type test_2_actual = Rebuild<[2, 3, 3, 5, 1, 1, 2]>;
//   ^?
type test_2_expected = [
  "🛹",
  "🛹",
  "🚲",
  "🚲",
  "🚲",
  "🛴",
  "🛴",
  "🛴",
  "🏄",
  "🏄",
  "🏄",
  "🏄",
  "🏄",
  "🛹",
  "🚲",
  "🛴",
  "🛴",
];
type test_2 = Expect<Equal<test_2_expected, test_2_actual>>;
