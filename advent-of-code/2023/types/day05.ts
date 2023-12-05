import {
  Trim,
  Split,
  Sum,
  Subtract,
  SortBy,
  SmallerThanOrEqual,
  MaxSafeInteger,
  Min,
  Chunk,
} from "./common";

type Input = Trim<typeof input>;

type Almanac = ParseAlmanac<Input>;

type Part1Helper<Maps extends Map[], Nums extends string[]> = Maps extends [
  infer Head extends Map,
  ...infer Rest extends Map[],
]
  ? Part1Helper<Rest, ApplyMap<Head, Nums>>
  : FoldMin<Nums>;

type Part1 = Part1Helper<Almanac["maps"], Almanac["seeds"]>;

type Chunks = Chunk<Almanac["seeds"], 2>;

type Pairs = {
  [Key in keyof Chunks as Key extends `${number}`
    ? Key
    : never]: Chunks[Key] extends [
    infer L extends string,
    infer R extends string,
  ]
    ? [[L, Subtract<Sum<L, R>, 1>]]
    : never;
};

// TS refuses to let me do this recursively, so yeah...
type Length = Chunks["length"];
//   ^?

type Part2 = FoldMin<
  [
    Part2Helper<Almanac["maps"], Pairs[0]>,
    Part2Helper<Almanac["maps"], Pairs[1]>,
  ]
>;

type Solution = [Part1, Part2];
//   ^?

type ApplyMap<
  M extends Map,
  Nums extends string[],
  Acc extends string[] = [],
> = Nums extends [infer Head extends string, ...infer Rest extends string[]]
  ? ApplyMap<M, Rest, [...Acc, MapValue<M, Head>]>
  : Acc;

type FoldMin<
  T extends string[],
  Acc extends string = `${MaxSafeInteger}`,
> = T extends [infer Head extends string, ...infer Rest extends string[]]
  ? FoldMin<Rest, Min<Acc, Head>>
  : Acc;

type PairNums<
  Nums extends string[],
  Acc extends Array<[string, string]> = [],
> = Nums extends [
  infer A extends string,
  infer B extends string,
  ...infer Rest extends string[],
]
  ? PairNums<Rest, [...Acc, [A, Subtract<Sum<A, B>, 1>]]>
  : Acc;

type Part2Helper<
  Maps extends Map[],
  Ranges extends Array<[string, string]>,
> = Maps extends [infer Head extends Map, ...infer Rest extends Map[]]
  ? Part2Helper<Rest, NextRanges<Head, Ranges>>
  : FoldMin<{
      [Key in keyof Ranges]: Ranges[Key][0];
    }>;

type NextRanges<
  M extends Map,
  Ranges extends Array<[string, string]>,
  Acc extends Array<[string, string]> = [],
> = Ranges extends [
  infer Head extends [string, string],
  ...infer Rest extends Array<[string, string]>,
]
  ? NextRange<M, Head> extends infer N extends Array<[string, string]>
    ? NextRanges<M, Rest, [...Acc, ...N]>
    : never
  : Acc;

type NextRange<M extends Map, Range extends [string, string]> = Range extends [
  infer L extends string,
  infer R extends string,
]
  ? FindRange<M, L> extends [unknown, infer LIdx extends string]
    ? FindRange<M, R> extends [unknown, infer RIdx extends string]
      ? BuildRanges<M, Range, LIdx, RIdx>
      : never
    : never
  : never;

type BuildRanges<
  M extends Map,
  V extends [string, string],
  L extends string,
  R extends string,
  Acc extends Array<[string, string]> = [],
  Index extends string = "0",
  Len = Subtract<R, L>,
> = SmallerThanOrEqual<L, R> extends false
  ? Acc
  : L extends keyof M
    ? M[L] extends infer C extends Range
      ? BuildRanges<
          M,
          V,
          Sum<L, 1>,
          R,
          [
            ...Acc,
            [
              Index extends "0" ? ApplyRange<C, V[0]> : C["dest"],
              Index extends Len
                ? ApplyRange<C, V[1]>
                : Sum<C["dest"], Subtract<C["to"], C["from"]>>,
            ],
          ],
          Sum<Index, 1>,
          Len
        >
      : never
    : never;

type ParseAlmanac<Input extends string> = Split<Input, "\n\n"> extends [
  infer Seeds extends string,
  ...infer Maps extends string[],
]
  ? {
      seeds: ParseSeeds<Seeds>;
      maps: ParseMaps<Maps>;
    }
  : never;

type ParseSeeds<Seeds extends string> = Seeds extends `seeds: ${infer Nums}`
  ? ParseNums<Nums>
  : never;

type ParseNums<S extends string> = ParseNumsHelper<Split<S, " ">>;

type ParseNumsHelper<
  Parts extends string[],
  Acc extends string[] = [],
> = Parts extends [infer Head extends string, ...infer Rest extends string[]]
  ? ParseNumsHelper<
      Rest,
      Trim<Head> extends `${infer N extends number}` ? [...Acc, `${N}`] : Acc
    >
  : Acc;

type Map = Range[];

type Range = {
  from: string;
  to: string;
  dest: string;
};

type ParseMaps<Maps extends string[], Acc extends Map[] = []> = Maps extends [
  infer Head extends string,
  ...infer Rest extends string[],
]
  ? ParseMaps<Rest, [...Acc, ParseMap<Head>]>
  : Acc;

type ParseMap<Map extends string> = Split<Map, "\n"> extends [
  string,
  ...infer Ranges extends string[],
]
  ? ParseRanges<Ranges>
  : never;

type ParseRanges<
  Ranges extends string[],
  Acc extends Range[] = [],
> = Ranges extends [infer Head extends string, ...infer Rest extends string[]]
  ? ParseRanges<Rest, [...Acc, ParseRange<Head>]>
  : PadRanges<SortBy<Acc, "from">>;

type ParseRange<Range extends string> = ParseNums<Range> extends [
  infer Dest extends string,
  infer Src extends string,
  infer Len extends string,
]
  ? {
      from: Src;
      to: Sum<Src, Subtract<Len, 1>>;
      dest: Dest;
    }
  : never;

type PrevRange<T extends Range[]> = T extends [
  ...Range[],
  infer Last extends Range,
]
  ? Sum<Last["to"], 1>
  : "0";

type PadRanges<
  Ranges extends Range[],
  Acc extends Range[] = [],
> = Ranges extends [infer Head extends Range, ...infer Rest extends Range[]]
  ? Subtract<Head["from"], 1> extends infer R extends string
    ? PrevRange<Acc> extends infer L extends string
      ? PadRanges<
          Rest,
          SmallerThanOrEqual<L, R> extends true
            ? [...Acc, { from: L; to: R; dest: L }, Head]
            : [...Acc, Head]
        >
      : never
    : never
  : [
      ...Acc,
      { from: PrevRange<Acc>; to: `${MaxSafeInteger}`; dest: PrevRange<Acc> },
    ];

type FindRange<
  Ranges extends Range[],
  N extends string,
  Index extends string = "0",
> = Ranges extends [infer Head extends Range, ...infer Rest extends Range[]]
  ? SmallerThanOrEqual<N, Head["to"]> extends true
    ? [Head, Index]
    : FindRange<Rest, N, Sum<Index, 1>>
  : never;

type ApplyRange<R extends Range, N extends string> = Sum<
  R["dest"],
  Subtract<N, R["from"]>
>;

type MapValue<Ranges extends Range[], N extends string> = FindRange<
  Ranges,
  N
> extends [infer R extends Range, string]
  ? ApplyRange<R, N>
  : never;

const input = `
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
`;
