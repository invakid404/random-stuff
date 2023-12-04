import {
  Range,
  FilterInv,
  Multiply,
  Split,
  Sum,
  Tail,
  Trim,
  Id,
} from "./common";

type Input = Split<Trim<typeof input>, "\n">;

type Cards = ParseCards<Input>;

type Part1<Cards extends Card[], Acc extends string = "0"> = Cards extends [
  infer Head extends Card,
  ...infer Rest extends Card[],
]
  ? Part1<Rest, Sum<Acc, Points<Head>>>
  : Acc;

type Part2<Cards extends Card[]> =
  Part2Helper<Cards> extends infer Amounts extends CardAmounts
    ? SumAmounts<Cards, Amounts>
    : never;

type Solution = [Part1<Cards>, Part2<Cards>];
//   ^?

type Points<C extends Card> = WinningNumbers<C> extends infer Nums extends
  number[]
  ? Nums extends []
    ? 0
    : PointsHelper<Tail<Nums>>
  : never;

type PointsHelper<
  Nums extends number[],
  Acc extends string = "1",
> = Nums extends [number, ...infer Rest extends number[]]
  ? PointsHelper<Rest, Multiply<Acc, 2>>
  : Acc;

type WinningNumbers<C extends Card> = FilterInv<
  C["numbers"],
  C["winningNumbers"][number]
>;

type CardAmounts = Record<number | string, string>;

type Part2Helper<
  Cards extends Card[],
  Acc extends CardAmounts = {},
> = Cards extends [infer Head extends Card, ...infer Rest extends Card[]]
  ? NextCards<Head> extends infer N extends string[]
    ? WinCards<N, GetAmount<Head["id"], Acc>, Acc> extends infer Next extends
        CardAmounts
      ? Part2Helper<Rest, Next>
      : never
    : never
  : Id<Acc>;

type NextCards<C extends Card> = WinningNumbers<C> extends infer N extends
  number[]
  ? Sum<C["id"], 1> extends infer Start extends string
    ? Range<Start, Sum<N["length"], Start>>
    : never
  : never;

type WinCards<
  N extends string[],
  Amount extends string,
  Acc extends CardAmounts,
> = N extends [infer Head extends string, ...infer Rest extends string[]]
  ? WinCards<
      Rest,
      Amount,
      Omit<Acc, Head> & {
        [Key in Head]: Sum<GetAmount<Key, Acc>, Amount>;
      }
    >
  : Acc;

type GetAmount<
  Id extends string,
  Acc extends CardAmounts,
> = Id extends keyof Acc
  ? Acc[Id] extends `${number | bigint}` | number | bigint
    ? Acc[Id]
    : "1"
  : "1";

type SumAmounts<
  Cards extends Card[],
  Amounts extends CardAmounts,
  Acc extends string = "0",
> = Cards extends [infer Head extends Card, ...infer Rest extends Card[]]
  ? SumAmounts<Rest, Amounts, Sum<Acc, GetAmount<Head["id"], Amounts>>>
  : Acc;

type Card = {
  id: string;
  winningNumbers: number[];
  numbers: number[];
};

type ParseCards<
  Lines extends string[],
  Acc extends Card[] = [],
> = Lines extends [infer Line extends string, ...infer Rest extends string[]]
  ? ParseCards<Rest, [...Acc, ParseCard<Line>]>
  : Acc;

type ParseCard<Line extends string> = Split<Line, ":"> extends [
  infer Id extends string,
  `${infer WinningNumbers}|${infer Numbers}`,
]
  ? {
      id: ParseId<Id>;
      winningNumbers: ParseNumbers<WinningNumbers>;
      numbers: ParseNumbers<Numbers>;
    }
  : never;

type ParseId<Id extends string> = Id extends `Card${infer Rest}`
  ? Trim<Rest>
  : never;

type ParseNumbers<Nums extends string> = ParseNumbersHelper<Split<Nums, " ">>;

type ParseNumbersHelper<
  Parts extends string[],
  Acc extends number[] = [],
> = Parts extends [infer Head, ...infer Rest extends string[]]
  ? ParseNumbersHelper<
      Rest,
      Head extends `${infer N extends number}` ? [...Acc, N] : Acc
    >
  : Acc;

const input = `
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
`;
