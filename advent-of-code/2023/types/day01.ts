import { Trim, Split, Chunk, ReverseStr, Sum } from "./common";

type Input = Split<Trim<typeof input>, "\n">;
type Chunks = Chunk<Input, 100>;

type Part1 = Solve<Chunks, Digit>;
type Part2 = Solve<Chunks, AnyDigit>;

type Solution = [Part1, Part2];
//  ^?

type WordToDigit = {
  one: "1";
  two: "2";
  three: "3";
  four: "4";
  five: "5";
  six: "6";
  seven: "7";
  eight: "8";
  nine: "9";
  zero: "0";
};

type Digit = WordToDigit[keyof WordToDigit];
type DigitWord = keyof WordToDigit;

type AnyDigit = Digit | DigitWord;

type Solve<
  Chunks extends string[][],
  Digits extends AnyDigit,
  Acc extends string = "0",
> = Chunks extends [
  infer Head extends string[],
  ...infer Rest extends string[][],
]
  ? Solve<Rest, Digits, Sum<Acc, SolveChunk<Head, Digits>>>
  : Acc;

type SolveChunk<
  Lines extends string[],
  Digits extends AnyDigit,
> = SolveChunkHelper<CalibrationValues<Lines, Digits>>;

type SolveChunkHelper<
  Values extends string[],
  Acc extends string = "0",
> = Values extends [infer Head extends string, ...infer Rest extends string[]]
  ? SolveChunkHelper<Rest, Sum<Acc, Head>>
  : Acc;

type CalibrationValues<
  Lines extends string[],
  Digits extends AnyDigit,
  Acc extends string[] = [],
> = Lines extends [infer Head extends string, ...infer Rest extends string[]]
  ? CalibrationValue<Head, Digits> extends infer V extends string
    ? CalibrationValues<Rest, Digits, [...Acc, V]>
    : never
  : Acc;

type CalibrationValue<T extends string, Digits extends AnyDigit> = FindPrefix<
  T,
  Digits
> extends infer A extends Digits
  ? FindSuffix<T, Digits> extends infer B extends Digits
    ? `${ToDigit<A>}${ToDigit<B>}`
    : never
  : never;

type FindPrefix<T extends string, Prefix extends string> = (
  Prefix extends unknown
    ? T extends `${Prefix}${string}`
      ? Prefix
      : never
    : never
) extends infer P extends Prefix
  ? [P] extends [never]
    ? T extends `${string}${infer Rest}`
      ? FindPrefix<Rest, Prefix>
      : never
    : P
  : never;

type FindSuffix<T extends string, Suffix extends string> = ReverseStr<
  FindPrefix<ReverseStr<T>, ReverseStr<Suffix>>
>;

type ToDigit<T extends AnyDigit> = T extends DigitWord ? WordToDigit[T] : T;

const input = `
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
`;
