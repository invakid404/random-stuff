/*
  31447 - CountReversePairs
  -------
  by jiangshan (@jiangshanmeta) #extreme

  ### Question

  Given an integer array nums, return the number of reverse pairs in the array.

  A reverse pair is a pair (i, j) where:

  * 0 <= i < j < nums.length and
  * nums[i] > nums[j].

  > View on GitHub: https://tsch.js.org/31447
*/

/* _____________ Your Code Here _____________ */
type CountReversePairs<T extends number[]> = CountReversePairsHelper<T>;

type CountReversePairsHelper<
  T extends number[],
  Acc extends readonly unknown[] = [],
> = T extends [infer Head extends number, ...infer Rest extends number[]]
  ? CountReversePairsHelper<Rest, [...Acc, ...CountOne<Head, Rest>]>
  : Acc['length'];

type CountOne<
  Head extends number,
  Rest extends number[],
  Acc extends readonly unknown[] = [],
> = Rest extends [infer Curr extends number, ...infer Next extends number[]]
  ? CountOne<
      Head,
      Next,
      Compare<Head, Curr> extends Comparison.Greater ? [...Acc, unknown] : Acc
    >
  : Acc;

type Init<T extends readonly unknown[]> = T extends [...infer Init, unknown]
  ? Init
  : [];

type Not<T extends boolean> = T extends true ? false : true;

type LastDigit<Digits extends readonly Digit[]> = Digits extends [
  ...Digit[],
  infer LastDigit,
]
  ? LastDigit
  : 0;

type ToDigits<Value extends string> = Value extends `${infer Head extends
  keyof StringToDigit}${infer Rest}`
  ? [StringToDigit[Head], ...ToDigits<Rest>]
  : [];

type Digit = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9;

type StringToDigit = { [Key in Digit as `${Key}`]: Key };

type Abs<T extends number | bigint | string> =
  `${T}` extends `-${infer Abs extends number | bigint}`
    ? [Abs, false]
    : [T, true];

type PadDigitSlices<
  Left extends readonly Digit[],
  Right extends readonly Digit[],
  LeftAcc extends readonly Digit[] = [],
  RightAcc extends readonly Digit[] = [],
> = BothEmpty<Left, Right> extends true
  ? [LeftAcc, RightAcc]
  : PadDigitSlices<
      Init<Left>,
      Init<Right>,
      [LastDigit<Left>, ...LeftAcc],
      [LastDigit<Right>, ...RightAcc]
    >;

type CompareDigitSlices<
  Left extends readonly Digit[],
  Right extends readonly Digit[],
> = PadDigitSlices<Left, Right> extends [
  infer PaddedLeft extends Digit[],
  infer PaddedRight extends Digit[],
]
  ? CompareDigitSlicesHelper<PaddedLeft, PaddedRight>
  : never;

type BothEmpty<
  Left extends readonly any[],
  Right extends readonly any[],
> = Left extends [] ? (Right extends [] ? true : false) : false;

type ComparisonRow<
  Value extends Digit,
  Acc extends readonly boolean[] = [],
  Bigger extends boolean = false,
> = Acc['length'] extends 10
  ? Acc
  : (
        Value extends Acc['length'] ? true : Bigger
      ) extends infer NewBigger extends boolean
    ? ComparisonRow<Value, [...Acc, NewBigger], NewBigger>
    : never;

type ComparisonTable = {
  [D in Digit]: ComparisonRow<D>;
};

type CompareDigitSlicesHelper<
  Left extends readonly Digit[],
  Right extends readonly Digit[],
> = BothEmpty<Left, Right> extends true
  ? true
  : Left extends [
        infer LeftHead extends Digit,
        ...infer LeftRest extends Digit[],
      ]
    ? Right extends [
        infer RightHead extends Digit,
        ...infer RightRest extends Digit[],
      ]
      ? LeftHead extends RightHead
        ? CompareDigitSlicesHelper<LeftRest, RightRest>
        : ComparisonTable[LeftHead][RightHead]
      : never
    : never;

type SmallerThanOrEqual<
  Left extends string | number | bigint,
  Right extends string | number | bigint,
> = Abs<Left> extends [
  infer AbsLeft extends string | number | bigint,
  infer SignLeft extends boolean,
]
  ? Abs<Right> extends [
      infer AbsRight extends string | number | bigint,
      infer SignRight extends boolean,
    ]
    ? SignLeft extends SignRight
      ? CompareDigitSlices<
          ToDigits<`${AbsLeft}`>,
          ToDigits<`${AbsRight}`>
        > extends infer Result extends boolean
        ? SignLeft extends true
          ? Result
          : Not<Result>
        : never
      : SignRight
    : never
  : never;

enum Comparison {
  Greater,
  Equal,
  Lower,
}

type Compare<
  Left extends string | number | bigint,
  Right extends string | number | bigint,
> = `${Left}` extends `${Right}`
  ? Comparison.Equal
  : SmallerThanOrEqual<Left, Right> extends true
    ? Comparison.Lower
    : Comparison.Greater;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<CountReversePairs<[5, 2, 6, 1]>, 4>>,
  Expect<Equal<CountReversePairs<[1, 2, 3, 4]>, 0>>,
  Expect<Equal<CountReversePairs<[-1, -1]>, 0>>,
  Expect<Equal<CountReversePairs<[-1]>, 0>>,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/31447/answer
  > View solutions: https://tsch.js.org/31447/solutions
  > More Challenges: https://tsch.js.org
*/
