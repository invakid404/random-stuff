/*
  34286 - Take Elements
  -------
  by Eirik Måseidvåg (@Eirmas) #hard #array

  ### Question

  Implement a type `Take<N, Arr>` that returns the first `N` elements from an array `Arr`. If `N` is negative, return the last `|N|` elements

  For example,
  ```ts
  type T0 = Take<2, [1, 2, 3]> // [1, 2]
  type T1 = Take<3, ['1', 2, true, false]> // ['1', 2, true]
  type T2 = Take<-2, [1, 2, 3]> // [2, 3]
  type T3 = Take<0, [1, 2, 3]> // []
  type T4 = Take<5, [1, 2, 3]> // [1, 2, 3]
  type T5 = Take<3, []> // []
  ```

  > View on GitHub: https://tsch.js.org/34286
*/

/* _____________ Your Code Here _____________ */
type TakeHelper<
  N extends number,
  T extends readonly unknown[],
  Acc extends readonly unknown[] = [],
> = Acc['length'] extends N
  ? Acc
  : T extends [infer Head, ...infer Rest]
    ? TakeHelper<N, Rest, [...Acc, Head]>
    : Acc;

type Reverse<
  T extends readonly unknown[],
  Acc extends readonly unknown[] = [],
> = T extends [infer Head, ...infer Rest] ? Reverse<Rest, [Head, ...Acc]> : Acc;

type Take<
  N extends number,
  T extends readonly unknown[],
> = `${N}` extends `-${infer Abs extends number}`
  ? Reverse<TakeHelper<Abs, Reverse<T>>>
  : TakeHelper<N, T>;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<Take<2, [1, 2, 3]>, [1, 2]>>,
  Expect<Equal<Take<3, ['1', 2, true, false]>, ['1', 2, true]>>,
  Expect<Equal<Take<-2, [1, 2, 3]>, [2, 3]>>,
  Expect<Equal<Take<0, [1, 2, 3]>, []>>,
  Expect<Equal<Take<5, [1, 2, 3]>, [1, 2, 3]>>,
  Expect<Equal<Take<3, []>, []>>,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/34286/answer
  > View solutions: https://tsch.js.org/34286/solutions
  > More Challenges: https://tsch.js.org
*/
