type BoxToys<T, N extends number> = N extends unknown ? Repeat<T, N> : never;

type Repeat<
  T,
  N extends number,
  Acc extends readonly unknown[] = [],
> = Acc["length"] extends N ? Acc : Repeat<T, N, [...Acc, T]>;

import { Expect, Equal } from "type-testing";

type test_doll_actual = BoxToys<"doll", 1>;
//   ^?
type test_doll_expected = ["doll"];
type test_doll = Expect<Equal<test_doll_expected, test_doll_actual>>;

type test_nutcracker_actual = BoxToys<"nutcracker", 3 | 4>;
//   ^?
type test_nutcracker_expected =
  | ["nutcracker", "nutcracker", "nutcracker"]
  | ["nutcracker", "nutcracker", "nutcracker", "nutcracker"];
type test_nutcracker = Expect<
  Equal<test_nutcracker_expected, test_nutcracker_actual>
>;
