export type Id<T> = { [Key in keyof T]: T[Key] };

export type Split<T extends string, D extends string> = SplitHelper<T, D>;

type SplitHelper<
  T extends string,
  D extends string,
  Acc extends string[] = [],
> = T extends ""
  ? D extends ""
    ? Acc
    : [...Acc, T]
  : T extends `${infer P}${D}${infer S}`
  ? SplitHelper<S, D, [...Acc, P]>
  : [...Acc, T];

type Whitespace = " " | "\n" | "\t";

export type TrimLeft<S extends string> = S extends `${Whitespace}${infer Rest}`
  ? TrimLeft<Rest>
  : S;

export type TrimRight<S extends string> = S extends `${infer Rest}${Whitespace}`
  ? TrimRight<Rest>
  : S;

export type Trim<S extends string> = TrimLeft<TrimRight<S>>;

export type Reverse<T extends readonly unknown[]> = ReverseHelper<T>;

type ReverseHelper<
  T extends readonly unknown[],
  Acc extends readonly unknown[] = [],
> = T extends [...infer Init, infer Last]
  ? ReverseHelper<Init, [...Acc, Last]>
  : Acc;

export type All<T extends boolean[]> = T extends [
  infer Head,
  ...infer Rest extends boolean[],
]
  ? Head extends false
    ? false
    : All<Rest>
  : true;

type Predecessor = {
  0: never;
  1: 0;
  2: 1;
  3: 2;
  4: 3;
  5: 4;
  6: 5;
  7: 6;
  8: 7;
  9: 8;
};

type Successor = {
  0: 1;
  1: 2;
  2: 3;
  3: 4;
  4: 5;
  5: 6;
  6: 7;
  7: 8;
  8: 9;
  // Invariant: the only digit with a successor of 0 is 9, meaning we don't
  // need any extra information to determine whether we need to carry. Getting
  // a 0 while adding two digits is enough.
  9: 0;
};

type Digit = keyof Successor;

type StringToDigit = { [key in Digit as `${key}`]: key };

// Compute the sum of two digits by recursively incrementing `Left` and
// decrementing `Right`, keeping track of the carry.
export type SumDigitsWithoutCarry<
  Left extends Digit,
  Right extends Digit,
  Carry extends boolean = false,
> = Right extends 0
  ? [Left, Carry]
  : Successor[Left] extends infer LeftSuccessor extends Digit
  ? SumDigitsWithoutCarry<
      LeftSuccessor,
      Predecessor[Right],
      LeftSuccessor extends 0 ? true : Carry
    >
  : never;

// Compute the sum of two digits, applying a carry at the end.
export type SumDigitsWithCarry<
  Left extends Digit,
  Right extends Digit,
> = SumDigitsWithoutCarry<Left, Right> extends [
  infer Result extends Digit,
  infer Carry extends boolean,
]
  ? Successor[Result] extends infer ResultSuccessor extends Digit
    ? [
        ResultSuccessor,
        // We have a carry if either the actual sum or the final increment
        // resulted in a carry.
        Carry extends true ? Carry : ResultSuccessor extends 0 ? true : false,
      ]
    : never
  : never;

export type SumDigits<
  Left extends Digit,
  Right extends Digit,
  Carry extends boolean,
> = Carry extends true
  ? SumDigitsWithCarry<Left, Right>
  : SumDigitsWithoutCarry<Left, Right>;

export type ToDigits<Value extends string> = Value extends `${infer Head extends
  keyof StringToDigit}${infer Rest}`
  ? [StringToDigit[Head], ...ToDigits<Rest>]
  : [];

export type BothEmpty<
  Left extends readonly any[],
  Right extends readonly any[],
> = Left extends [] ? (Right extends [] ? true : false) : false;

export type Head<T extends readonly unknown[]> = T extends [
  infer Head,
  ...unknown[],
]
  ? Head
  : never;

export type Tail<T extends readonly unknown[]> = T extends [
  unknown,
  ...infer Tail,
]
  ? Tail
  : never;

export type Init<T extends readonly unknown[]> = T extends [
  ...infer Init,
  unknown,
]
  ? Init
  : [];

// Get the last digit of a digit slice, returning 0 if empty.
//
// This export type allows us to effectively "pad" both numbers to be the same length
// without going through all the trouble.
export type LastDigit<Digits extends readonly Digit[]> = Digits extends [
  ...Digit[],
  infer LastDigit,
]
  ? LastDigit
  : 0;

export type SumDigitSlices<
  Left extends readonly Digit[],
  Right extends readonly Digit[],
  Carry extends boolean = false,
> = BothEmpty<Left, Right> extends true
  ? Carry extends true
    ? [1] // If we still have a carry, return a leading one.
    : []
  : SumDigits<LastDigit<Left>, LastDigit<Right>, Carry> extends [
      infer Result,
      infer Carry extends boolean,
    ]
  ? [...SumDigitSlices<Init<Left>, Init<Right>, Carry>, Result]
  : never;

export type DigitsToString<Digits extends readonly Digit[]> = Digits extends [
  infer Head extends Digit,
  ...infer Tail extends readonly Digit[],
]
  ? `${Head}${DigitsToString<Tail>}`
  : "";

// A matrix, containing the result of the multiplication of any two digits,
// represented as a digit slice.
export type MultiplicationTable = { [key in Digit]: MultiplicationRow<key> };

// Computes the row of the multiplication table for a given digit by
// repeatedly adding the digit to the previous result.
export type MultiplicationRow<
  Value extends Digit,
  Accumulator extends readonly (readonly Digit[])[] = [[0]],
> = Accumulator["length"] extends 10
  ? Accumulator
  : Accumulator extends [...any[], infer Last extends readonly Digit[]]
  ? SumDigitSlices<Last, [Value]> extends infer Result extends readonly Digit[]
    ? MultiplicationRow<Value, [...Accumulator, Result]>
    : never
  : never;

// Multiplies a value with a single digit factor.
export type MultiplyWithDigit<
  Value extends readonly Digit[],
  Factor extends Digit,
  Result extends readonly Digit[] = [],
  Carry extends Digit = 0,
> = Value extends []
  ? Carry extends 0
    ? Result
    : [Carry, ...Result] // If we still have a carry, append it to the front.
  : SumDigitSlices<
      // Multiply the last digit with the factor...
      MultiplicationTable[LastDigit<Value>][Factor],
      // ...and add the carry to it.
      [Carry]
    > extends infer Current extends readonly Digit[]
  ? // If we get a two-digit result, the first digit is the carry.
    Current extends [infer Carry extends Digit, infer Out extends Digit]
    ? MultiplyWithDigit<Init<Value>, Factor, [Out, ...Result], Carry>
    : MultiplyWithDigit<Init<Value>, Factor, [...Current, ...Result], 0>
  : never;

// Multiplies two digit slices, applying the method taught in school:
// multiply the left number by each of the digits of the right one in reverse
// order, shifting every next result by one digit to the left, then add up all
// intermediate values.
export type MultiplyDigitSlices<
  Left extends readonly Digit[],
  Right extends readonly Digit[],
  // Zeros to shift the result of the current single-digit multiplication by.
  // Essentially acts like multiplying by a factor of 10.
  Padding extends readonly Digit[] = [],
> = Right extends []
  ? [0]
  : SumDigitSlices<
      [...MultiplyWithDigit<Left, LastDigit<Right>>, ...Padding],
      MultiplyDigitSlices<Left, Init<Right>, [...Padding, 0]>
    >;

// Removes all leading zeros from a digit slice.
export type TrimLeadingZeros<Value extends readonly Digit[]> = Value extends [0]
  ? Value
  : Value extends [0, ...infer Rest extends readonly Digit[]]
  ? TrimLeadingZeros<Rest>
  : Value;

export type Sum<
  Left extends number | bigint | string,
  Right extends number | bigint | string,
> = SumDigitSlices<
  ToDigits<`${Left}`>,
  ToDigits<`${Right}`>
> extends infer Result extends readonly Digit[]
  ? DigitsToString<Result>
  : never;

export type Multiply<
  Left extends string | number | bigint,
  Right extends string | number | bigint,
> = MultiplyDigitSlices<
  ToDigits<`${Left}`>,
  ToDigits<`${Right}`>
> extends infer Result extends readonly Digit[]
  ? DigitsToString<TrimLeadingZeros<Result>>
  : never;

export type ComparisonRow<
  Value extends Digit,
  Acc extends readonly boolean[] = [],
  Bigger extends boolean = false,
> = Acc["length"] extends 10
  ? Acc
  : (
      Value extends Acc["length"] ? true : Bigger
    ) extends infer NewBigger extends boolean
  ? ComparisonRow<Value, [...Acc, NewBigger], NewBigger>
  : never;

type ComparisonTable = {
  [D in Digit]: ComparisonRow<D>;
};

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

export type SmallerThan<
  Left extends string | number | bigint,
  Right extends string | number | bigint,
> = `${Left}` extends `${Right}` ? false : SmallerThanOrEqual<Left, Right>;

export type SmallerThanOrEqual<
  Left extends string | number | bigint,
  Right extends string | number | bigint,
> = CompareDigitSlices<ToDigits<`${Left}`>, ToDigits<`${Right}`>>;

export type Min<
  Left extends string | number | bigint,
  Right extends string | number | bigint,
> = SmallerThanOrEqual<Left, Right> extends true ? Left : Right;

export type Max<
  Left extends string | number | bigint,
  Right extends string | number | bigint,
> = SmallerThanOrEqual<Left, Right> extends true ? Right : Left;
