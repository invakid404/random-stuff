/*
  741 - Sort
  -------
  by Sg (@suica) #extreme #infer #array
  
  ### Question
  
  In this challenge, you are required to sort natural number arrays in either ascend order or descent order.
  
  Ascend order examples:
  ```ts
  Sort<[]> // []
  Sort<[1]> // [1]
  Sort<[2, 4, 7, 6, 6, 6, 5, 8, 9]> //  [2, 4, 5, 6, 6, 6, 7, 8, 9]
  ```
  
  The `Sort` type should also accept a boolean type. When it is `true`, the sorted result should be in descent order. Some examples:
  
  ```ts
  Sort<[3, 2, 1], true> // [3, 2, 1]
  Sort<[3, 2, 0, 1, 0, 0, 0], true> // [3, 2, 1, 0, 0, 0, 0]
  ```
  
  Extra challenges:
  1. Support natural numbers with 15+ digits.
  2. Support float numbers.
  
  > View on GitHub: https://tsch.js.org/741
*/

/* _____________ Your Code Here _____________ */
enum Comparison {
  Greater,
  Equal,
  Lower,
}

type Tail<Slice extends readonly any[]> = Slice extends [any, ...infer Tail]
  ? Tail
  : [];

// Represents a number as the length of a slice by repeatedly appending `any`.
type AsSlice<
  A extends number,
  Accumulator extends readonly any[] = [],
> = A extends Accumulator['length']
  ? Accumulator
  : AsSlice<A, [...Accumulator, any]>;

type StringToNumber<T extends string, A extends any[] = []> = T extends keyof [
  0,
  ...A,
]
  ? A['length']
  : StringToNumber<T, [0, ...A]>;

type Abs<A extends number> = `${A}` extends `-${infer Abs}`
  ? [true, StringToNumber<Abs>]
  : [false, A];

// Compares the length of two slices recursively.
//
// At each step, it removes the head of `ASlice` and appends it to
// `Accumulator`. If at any point `Accumulator` is of equal length to
// `BSlice`, then `ASlice` must be at least as long as `BSlice`.
// If `ASlice` becomes empty before the above is true, then it's shorter.
type IsLongerOrEqual<
  ASlice extends readonly any[],
  BSlice extends readonly any[],
  Accumulator extends readonly any[] = [],
> = Accumulator['length'] extends BSlice['length']
  ? true
  : ASlice extends []
  ? false
  : IsLongerOrEqual<Tail<ASlice>, BSlice, [...Accumulator, ASlice[0]]>;

// Compares two positive numbers. Assumes A != B.
type ComparatorPos<
  A extends number,
  B extends number,
> = AsSlice<A> extends infer ASlice extends readonly any[]
  ? AsSlice<B> extends infer BSlice extends readonly any[]
    ? IsLongerOrEqual<ASlice, BSlice> extends true
      ? Comparison.Greater
      : Comparison.Lower
    : never
  : never;

type Invert<X> = X extends Comparison.Lower
  ? Comparison.Greater
  : Comparison.Lower;

type Comparator<A extends number, B extends number> = A extends B
  ? Comparison.Equal
  : Abs<A> extends [infer ANeg extends boolean, infer AAbs extends number]
  ? Abs<B> extends [infer BNeg extends boolean, infer BAbs extends number]
    ? ANeg extends BNeg
      ? // If both numbers have the same sign, compare their absolute value.
        ComparatorPos<AAbs, BAbs> extends infer ResultAbs
        ? // If we're comparing negative numbers, the result must be inverted.
          ANeg extends true
          ? Invert<ResultAbs>
          : ResultAbs
        : never
      : // If the two numbers have different signs, the positive one is bigger.
      ANeg extends true
      ? Comparison.Lower
      : Comparison.Greater
    : never
  : never;

type CompareWithDirection<
  A extends number,
  B extends number,
  Direction extends boolean,
> = Comparator<A, B> extends infer Result
  ? Result extends Comparison.Equal
    ? Result
    : Direction extends false
    ? Result
    : Result extends Comparison.Lower
    ? Comparison.Greater
    : Comparison.Lower
  : never;

type Split<
  Arr extends readonly any[],
  Index extends number,
  InitAccumulator extends readonly any[] = [],
> = Arr extends []
  ? [InitAccumulator, Arr]
  : InitAccumulator['length'] extends Index
  ? [InitAccumulator, Arr]
  : Split<Tail<Arr>, Index, [...InitAccumulator, Arr[0]]>;

type Drop<Array extends readonly any[], Index extends number> = Split<
  Array,
  Index
> extends [
  infer Left extends readonly any[],
  infer Right extends readonly any[],
]
  ? [...Left, ...Tail<Right>]
  : never;

type Partition<
  Array extends readonly number[],
  PivotIndex extends number,
  Direction extends boolean,
> = PartitionWithValue<Drop<Array, PivotIndex>, Array[PivotIndex], Direction>;

type PartitionWithValue<
  Array extends readonly number[],
  Pivot extends number,
  Direction extends boolean,
  Left extends readonly any[] = [],
  Right extends readonly any[] = [],
> = Array extends [
  infer Head extends number,
  ...infer Tail extends readonly number[],
]
  ? CompareWithDirection<Head, Pivot, Direction> extends Comparison.Lower
    ? PartitionWithValue<Tail, Pivot, Direction, [...Left, Head], Right>
    : PartitionWithValue<Tail, Pivot, Direction, Left, [...Right, Head]>
  : [Left, Right];

type Sort<
  Array extends readonly number[],
  Direction extends boolean = false,
> = Array extends []
  ? []
  : Partition<Array, 0, Direction> extends [
      infer Left extends readonly number[],
      infer Right extends readonly number[],
    ]
  ? [...Sort<Left, Direction>, Array[0], ...Sort<Right, Direction>]
  : never;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<Sort<[]>, []>>,
  Expect<Equal<Sort<[1]>, [1]>>,
  Expect<Equal<Sort<[2, 1]>, [1, 2]>>,
  Expect<Equal<Sort<[0, 0, 0]>, [0, 0, 0]>>,
  Expect<Equal<Sort<[1, 2, 3]>, [1, 2, 3]>>,
  Expect<Equal<Sort<[3, 2, 1]>, [1, 2, 3]>>,
  Expect<Equal<Sort<[3, 2, 1, 2]>, [1, 2, 2, 3]>>,
  Expect<Equal<Sort<[3, 2, 0, 1, 0, 0, 0]>, [0, 0, 0, 0, 1, 2, 3]>>,
  Expect<Equal<Sort<[2, 4, 7, 6, 6, 6, 5, 8, 9]>, [2, 4, 5, 6, 6, 6, 7, 8, 9]>>,
  Expect<Equal<Sort<[1, 1, 2, 1, 1, 1, 1, 1, 1]>, [1, 1, 1, 1, 1, 1, 1, 1, 2]>>,
  Expect<Equal<Sort<[], true>, []>>,
  Expect<Equal<Sort<[1], true>, [1]>>,
  Expect<Equal<Sort<[2, 1], true>, [2, 1]>>,
  Expect<Equal<Sort<[0, 0, 0], true>, [0, 0, 0]>>,
  Expect<Equal<Sort<[1, 2, 3], true>, [3, 2, 1]>>,
  Expect<Equal<Sort<[3, 2, 1], true>, [3, 2, 1]>>,
  Expect<Equal<Sort<[3, 2, 1, 2], true>, [3, 2, 2, 1]>>,
  Expect<Equal<Sort<[3, 2, 0, 1, 0, 0, 0], true>, [3, 2, 1, 0, 0, 0, 0]>>,
  Expect<
    Equal<Sort<[2, 4, 7, 6, 6, 6, 5, 8, 9], true>, [9, 8, 7, 6, 6, 6, 5, 4, 2]>
  >,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/741/answer
  > View solutions: https://tsch.js.org/741/solutions
  > More Challenges: https://tsch.js.org
*/
