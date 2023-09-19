/*
  9384 - Maximum
  -------
  by ch3cknull (@ch3cknull) #hard #array

  ### Question

  ### Description

  Implement the type `Maximum`, which takes an input type `T`, and returns the maximum value in `T`.

  If `T` is an empty array, it returns `never`. **Negative numbers** are not considered.

  For example:

  ```ts
  Maximum<[]> // never
  Maximum<[0, 2, 1]> // 2
  Maximum<[1, 20, 200, 150]> // 200
  ```

  ### Advanced

  Can you implement type `Minimum` inspired by `Maximum`?

  > View on GitHub: https://tsch.js.org/9384
*/

/* _____________ Your Code Here _____________ */
type NumberToSlice<
  T extends number,
  Acc extends readonly unknown[] = [],
> = Acc['length'] extends T ? Acc : NumberToSlice<T, [...Acc, unknown]>;

type IsLongerOrEqual<
  ASlice extends readonly any[],
  BSlice extends readonly any[],
  Accumulator extends readonly any[] = [],
> = Accumulator['length'] extends BSlice['length']
  ? true
  : ASlice extends []
  ? false
  : IsLongerOrEqual<Tail<ASlice>, BSlice, [...Accumulator, ASlice[0]]>;

type MaximumSlice<
  T extends ReadonlyArray<readonly unknown[]>,
  Current extends readonly unknown[] = never,
> = T extends [
  infer Head extends readonly unknown[],
  ...infer Rest extends ReadonlyArray<readonly unknown[]>,
]
  ? [Current] extends [never]
    ? MaximumSlice<Rest, Head>
    : IsLongerOrEqual<Head, Current> extends true
    ? MaximumSlice<Rest, Head>
    : MaximumSlice<Rest, Current>
  : Current;

type Maximum<T extends readonly number[]> = MaximumSlice<{
  [Key in keyof T]: NumberToSlice<T[Key]>;
}>['length'];

type Tail<Slice extends readonly any[]> = Slice extends [any, ...infer Tail]
  ? Tail
  : [];

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<Maximum<[]>, never>>,
  Expect<Equal<Maximum<[0, 2, 1]>, 2>>,
  Expect<Equal<Maximum<[1, 20, 200, 150]>, 200>>,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/9384/answer
  > View solutions: https://tsch.js.org/9384/solutions
  > More Challenges: https://tsch.js.org
*/
