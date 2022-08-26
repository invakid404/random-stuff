/*
  274 - Integers Comparator
  -------
  by Pig Fang (@g-plane) #extreme #template-literal #math
  
  ### Question
  
  Implement a type-level integers comparator. We've provided an enum for indicating the comparison result, like this:
  
  - If `a` is greater than `b`, type should be `Comparison.Greater`.
  - If `a` and `b` are equal, type should be `Comparison.Equal`.
  - If `a` is lower than `b`, type should be `Comparison.Lower`.
  
  **Note that `a` and `b` can be positive integers or negative integers or zero, even one is positive while another one is negative.**
  
  > View on GitHub: https://tsch.js.org/274
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

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<Comparator<5, 5>, Comparison.Equal>>,
  Expect<Equal<Comparator<5, 6>, Comparison.Lower>>,
  Expect<Equal<Comparator<5, 8>, Comparison.Lower>>,
  Expect<Equal<Comparator<5, 0>, Comparison.Greater>>,
  Expect<Equal<Comparator<-5, 0>, Comparison.Lower>>,
  Expect<Equal<Comparator<0, 0>, Comparison.Equal>>,
  Expect<Equal<Comparator<0, -5>, Comparison.Greater>>,
  Expect<Equal<Comparator<5, -3>, Comparison.Greater>>,
  Expect<Equal<Comparator<5, -7>, Comparison.Greater>>,
  Expect<Equal<Comparator<-5, -7>, Comparison.Greater>>,
  Expect<Equal<Comparator<-5, -3>, Comparison.Lower>>,
  Expect<Equal<Comparator<-25, -30>, Comparison.Greater>>,
  Expect<Equal<Comparator<15, -23>, Comparison.Greater>>,
  Expect<Equal<Comparator<40, 37>, Comparison.Greater>>,
  Expect<Equal<Comparator<-36, 36>, Comparison.Lower>>,
  Expect<Equal<Comparator<27, 27>, Comparison.Equal>>,
  Expect<Equal<Comparator<-38, -38>, Comparison.Equal>>,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/274/answer
  > View solutions: https://tsch.js.org/274/solutions
  > More Challenges: https://tsch.js.org
*/
