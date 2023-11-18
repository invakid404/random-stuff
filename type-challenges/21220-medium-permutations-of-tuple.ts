/*
  21220 - Permutations of Tuple
  -------
  by null (@gaac510) #medium #union #tuple #conditional type #recursion

  ### Question

  Given a generic tuple type `T extends unknown[]`, write a type which produces all permutations of `T` as a union.

  For example:

  ```ts
  PermutationsOfTuple<[1, number, unknown]>
   * Should return:
   * | [1, number, unknown]
   * | [1, unknown, number]
   * | [number, 1, unknown]
   * | [unknown, 1, number]
   * | [number, unknown, 1]
   * | [unknown, number ,1]
  ```

  > View on GitHub: https://tsch.js.org/21220
*/

/* _____________ Your Code Here _____________ */
type PermutationsOfTuple<T extends unknown[]> = T extends []
  ? []
  : {
      [Key in keyof T]: Key extends `${infer Index extends number}`
        ? PermutationsOfTuple<
            RemoveAt<T, Index>
          > extends infer Permutations extends unknown[]
          ? [T[Key], ...Permutations]
          : never
        : never;
    }[number];

type RemoveAt<
  T extends readonly unknown[],
  N extends number,
  Acc extends readonly unknown[] = [],
> = T extends [infer Head, ...infer Rest]
  ? Acc['length'] extends N
    ? [...Acc, ...Rest]
    : RemoveAt<Rest, N, [...Acc, Head]>
  : Acc;

/* _____________ Test Cases _____________ */
import type { Equal, Expect, ExpectFalse } from '@type-challenges/utils';

type cases = [
  Expect<Equal<PermutationsOfTuple<[]>, []>>,
  Expect<Equal<PermutationsOfTuple<[any]>, [any]>>,
  Expect<
    Equal<PermutationsOfTuple<[any, unknown]>, [any, unknown] | [unknown, any]>
  >,
  Expect<
    Equal<
      PermutationsOfTuple<[any, unknown, never]>,
      | [any, unknown, never]
      | [unknown, any, never]
      | [unknown, never, any]
      | [any, never, unknown]
      | [never, any, unknown]
      | [never, unknown, any]
    >
  >,
  Expect<
    Equal<
      PermutationsOfTuple<[1, number, unknown]>,
      | [1, number, unknown]
      | [1, unknown, number]
      | [number, 1, unknown]
      | [unknown, 1, number]
      | [number, unknown, 1]
      | [unknown, number, 1]
    >
  >,
  ExpectFalse<Equal<PermutationsOfTuple<[1, number, unknown]>, [unknown]>>,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/21220/answer
  > View solutions: https://tsch.js.org/21220/solutions
  > More Challenges: https://tsch.js.org
*/
