export type Id<T> = { [Key in keyof T]: T[Key] };

export type Not<T extends boolean> = T extends true ? false : true;

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

export type Join<
  T extends readonly string[],
  U extends string | number,
> = T extends [infer Head extends string, ...infer Rest extends string[]]
  ? `${Head}${JoinHelper<Rest, U>}`
  : "";

type JoinHelper<
  T extends readonly string[],
  U extends string | number,
> = T extends [
  infer Head extends string,
  ...infer Rest extends readonly string[],
]
  ? `${U}${Head}${JoinHelper<Rest, U>}`
  : "";

export type Chunk<
  T extends readonly unknown[],
  Size extends number,
> = ChunkHelper<T, Size>;

type ChunkHelper<
  T extends readonly unknown[],
  Size extends number,
  Acc extends readonly unknown[] = [],
> = T["length"] extends 0
  ? Acc
  : NextChunk<T, Size> extends [
      infer Chunk,
      infer Rest extends readonly unknown[],
    ]
  ? ChunkHelper<Rest, Size, [...Acc, Chunk]>
  : never;

type NextChunk<
  T extends readonly unknown[],
  Size extends number,
  Acc extends readonly unknown[] = [],
> = Acc["length"] extends Size
  ? [Acc, T]
  : T extends [infer Head, ...infer Rest]
  ? NextChunk<Rest, Size, [...Acc, Head]>
  : [Acc, T];

export type Filter<
  T extends unknown[],
  S,
  Acc extends unknown[] = [],
> = T extends [infer Head, ...infer Rest]
  ? Filter<Rest, S, Head extends S ? Acc : [...Acc, Head]>
  : Acc;

export type FilterInv<T extends unknown[], S> = Filter<
  T,
  Exclude<T[number], S>
>;

export type UnionToIntersection<U> = (
  U extends any ? (arg: U) => any : never
) extends (arg: infer I) => void
  ? I
  : never;

// Retrieves the last element of a union by wrapping every element in a dummy
// function type, then inferring the argument.
//
// Similarly to how TypeScript handles overloaded functions, the conditional
// targets the last element of the intersection specifically, resulting in the
// last element of the union.
export type LastInUnion<U> = UnionToIntersection<
  U extends any ? (arg: U) => void : never
> extends (arg: infer Last) => void
  ? Last
  : never;

export type UnionToTuple<U> = [U] extends [never]
  ? []
  : LastInUnion<U> extends infer Last
  ? [...UnionToTuple<Exclude<U, Last>>, Last]
  : never;

export type ObjectEntries<T> = {
  [Key in keyof T]-?: [
    Key,
    T[Key] extends infer Value | undefined ? Value : undefined,
  ];
}[keyof T];

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

export type ReverseStr<T extends string> = ReverseStrHelper<T>;

type ReverseStrHelper<
  T extends string,
  Acc extends string = "",
> = T extends `${infer Head}${infer Rest}`
  ? ReverseStrHelper<Rest, `${Head}${Acc}`>
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
  0: 9;
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

type SubtractDigitsWithoutCarry<
  Left extends Digit,
  Right extends Digit,
  Carry extends boolean = false,
> = Right extends 0
  ? [Left, Carry]
  : Predecessor[Left] extends infer LeftPredecessor extends Digit
  ? SubtractDigitsWithoutCarry<
      LeftPredecessor,
      Predecessor[Right],
      LeftPredecessor extends 9 ? true : Carry
    >
  : never;

type SubtractDigitsWithCarry<
  Left extends Digit,
  Right extends Digit,
> = Predecessor[Left] extends infer LeftPredecessor extends Digit
  ? SubtractDigitsWithoutCarry<LeftPredecessor, Right> extends [
      infer Result extends Digit,
      infer Carry extends boolean,
    ]
    ? [Result, LeftPredecessor extends 9 ? true : Carry]
    : never
  : never;

type SubtractDigits<
  Left extends Digit,
  Right extends Digit,
  Carry extends boolean,
> = Carry extends true
  ? SubtractDigitsWithCarry<Left, Right>
  : SubtractDigitsWithoutCarry<Left, Right>;

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
  : [];

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

type SubtractDigitSlices<
  Left extends readonly Digit[],
  Right extends readonly Digit[],
  Carry extends boolean = false,
> = BothEmpty<Left, Right> extends true
  ? Carry extends true
    ? // If we ran out of digits and we still need to carry, the result is negative.
      never
    : []
  : SubtractDigits<LastDigit<Left>, LastDigit<Right>, Carry> extends [
      infer Result,
      infer Carry extends boolean,
    ]
  ? [...SubtractDigitSlices<Init<Left>, Init<Right>, Carry>, Result]
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

export type Abs<T extends number | bigint | string> =
  `${T}` extends `-${infer Abs extends number | bigint}`
    ? [Abs, false]
    : [T, true];

export type Sum<
  Left extends number | bigint | string,
  Right extends number | bigint | string,
> = SumDigitSlices<
  ToDigits<`${Left}`>,
  ToDigits<`${Right}`>
> extends infer Result extends readonly Digit[]
  ? DigitsToString<Result>
  : never;

export type Subtract<
  Left extends number | bigint | string,
  Right extends number | bigint | string,
> = SmallerThan<Left, Right> extends infer Negative extends boolean
  ? SubtractDigitSlices<
      ToDigits<Negative extends true ? `${Right}` : `${Left}`>,
      ToDigits<Negative extends true ? `${Left}` : `${Right}`>
    > extends infer Result extends readonly Digit[]
    ? DigitsToString<TrimLeadingZeros<Result>> extends infer S extends string
      ? `${Negative extends true ? "-" : ""}${S}`
      : never
    : never
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

export type Min<
  Left extends string | number | bigint,
  Right extends string | number | bigint,
> = SmallerThanOrEqual<Left, Right> extends true ? Left : Right;

export type Max<
  Left extends string | number | bigint,
  Right extends string | number | bigint,
> = SmallerThanOrEqual<Left, Right> extends true ? Right : Left;

export type Range<
  L extends string | number | bigint,
  R extends string | number | bigint,
> = SmallerThanOrEqual<L, R> extends false
  ? never
  : RangeHelper<`${L}`, `${R}`>;

export type RangeHelper<
  L extends string,
  R extends string,
  Acc extends string[] = [],
> = L extends R ? Acc : RangeHelper<Sum<L, 1>, R, [...Acc, L]>;

type SplitAt<
  Arr extends readonly any[],
  Index extends number,
  InitAccumulator extends readonly any[] = [],
> = Arr extends []
  ? [InitAccumulator, Arr]
  : InitAccumulator["length"] extends Index
  ? [InitAccumulator, Arr]
  : SplitAt<Tail<Arr>, Index, [...InitAccumulator, Arr[0]]>;

type Drop<Array extends readonly any[], Index extends number> = SplitAt<
  Array,
  Index
> extends [
  infer Left extends readonly any[],
  infer Right extends readonly any[],
]
  ? [...Left, ...Tail<Right>]
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

type CompareWithDirection<
  Left extends string | number | bigint,
  Right extends string | number | bigint,
  Direction extends boolean,
> = Compare<Left, Right> extends infer Result
  ? Result extends Comparison.Equal
    ? Result
    : Direction extends false
    ? Result
    : Result extends Comparison.Lower
    ? Comparison.Greater
    : Comparison.Lower
  : never;

type Partition<
  Array extends readonly Sortable[],
  PivotIndex extends number,
  Direction extends boolean,
> = PartitionWithValue<Drop<Array, PivotIndex>, Array[PivotIndex], Direction>;

type PartitionWithValue<
  Array extends readonly Sortable[],
  Pivot extends Sortable,
  Direction extends boolean,
  Left extends readonly any[] = [],
  Right extends readonly any[] = [],
> = Array extends [
  infer Head extends Sortable,
  ...infer Tail extends readonly Sortable[],
]
  ? CompareSortables<Head, Pivot, Direction> extends Comparison.Lower
    ? PartitionWithValue<Tail, Pivot, Direction, [...Left, Head], Right>
    : PartitionWithValue<Tail, Pivot, Direction, Left, [...Right, Head]>
  : [Left, Right];

type Sort<
  Array extends readonly Sortable[],
  Direction extends boolean = false,
> = Array extends []
  ? []
  : Partition<Array, 0, Direction> extends [
      infer Left extends readonly Sortable[],
      infer Right extends readonly Sortable[],
    ]
  ? [...Sort<Left, Direction>, Array[0], ...Sort<Right, Direction>]
  : never;

type Sortable = number | { value: number };

type SortableToNumber<T extends Sortable> = T extends object ? T["value"] : T;

type CompareSortables<
  Left extends Sortable,
  Right extends Sortable,
  Direction extends boolean,
> = CompareWithDirection<
  SortableToNumber<Left>,
  SortableToNumber<Right>,
  Direction
>;

declare const sortSymbol: unique symbol;

export type SortBy<
  Array extends readonly object[],
  Key extends keyof Array[number],
> = Sort<ToSortable<Array, Key>> extends infer Sorted extends Sortable[]
  ? RestoreValues<Sorted>
  : never;

type ToSortable<
  Array extends readonly object[],
  Key extends keyof Array[number],
  Acc extends Sortable[] = [],
> = Array extends [
  infer Head extends object,
  ...infer Rest extends readonly object[],
]
  ? Key extends keyof Head
    ? Head[Key] extends number | `${number}`
      ? ToSortable<
          Rest,
          Key,
          [
            ...Acc,
            { value: ToNumber<Head[Key]> } & {
              [Key in typeof sortSymbol]?: Head;
            },
          ]
        >
      : never
    : never
  : Acc;

type ToNumber<T extends string | number> = T extends number
  ? T
  : T extends `${infer N extends number}`
  ? N
  : never;

type RestoreValues<
  T extends Sortable[],
  Acc extends readonly object[] = [],
> = T extends [infer Head extends Sortable, ...infer Rest extends Sortable[]]
  ? RestoreValue<Head> extends infer V extends object
    ? RestoreValues<Rest, [...Acc, V]>
    : never
  : Acc;

type RestoreValue<T extends Sortable> = typeof sortSymbol extends keyof T
  ? Exclude<T[typeof sortSymbol], undefined>
  : never;

export type MaxSafeInteger = 9007199254740991;
