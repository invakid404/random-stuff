type IsEmpty<T extends readonly unknown[]> = T extends [] ? true : false;

type BothEmpty<
  T extends readonly unknown[],
  U extends readonly unknown[],
> = T extends [] ? (U extends [] ? true : false) : false;

type Tail<T extends readonly unknown[]> = T extends [unknown, ...infer Rest]
  ? Rest
  : [];

type CompareArrayLengthHelper<
  T extends readonly unknown[],
  U extends readonly unknown[],
> = BothEmpty<T, U> extends true
  ? 0
  : IsEmpty<T> extends true
    ? -1
    : IsEmpty<U> extends true
      ? 1
      : CompareArrayLengthHelper<Tail<T>, Tail<U>>;

type CompareArrayLength<
  T extends readonly unknown[],
  U extends readonly unknown[],
> = CompareArrayLengthHelper<T, U>;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<CompareArrayLength<[1, 2, 3, 4], [5, 6]>, 1>>,
  Expect<Equal<CompareArrayLength<[1, 2], [3, 4, 5, 6]>, -1>>,
  Expect<Equal<CompareArrayLength<[], []>, 0>>,
  Expect<Equal<CompareArrayLength<[1, 2, 3], [4, 5, 6]>, 0>>,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/34007/answer
  > View solutions: https://tsch.js.org/34007/solutions
  > More Challenges: https://tsch.js.org
*/
