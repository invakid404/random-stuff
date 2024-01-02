/*
  30958 - Pascal's triangle
  -------
  by Aswin S Vijay (@aswinsvijay) #medium #array #math

  ### Question

  Given a number N, construct the Pascal's triangle with N rows.
  [Wikipedia](https://en.wikipedia.org/wiki/Pascal%27s_triangle)

  > View on GitHub: https://tsch.js.org/30958
*/

/* _____________ Your Code Here _____________ */
type Pascal<N extends number> = PascalHelper<N>;

type PascalHelper<
  N extends number,
  Prev extends ReadonlyArray<readonly unknown[]> = [[unknown]],
  Curr extends ReadonlyArray<readonly unknown[]> = [],
  Acc extends ReadonlyArray<readonly number[]> = [ToNumberTuple<Prev>],
> = Acc['length'] extends N
  ? Acc
  : Tail<Curr>['length'] extends Acc['length']
    ? PascalHelper<N, Curr, [], [...Acc, ToNumberTuple<Curr>]>
    : Curr['length'] extends infer Index extends number
      ? Get<Prev, MinusOne<Index>> extends infer Left extends readonly unknown[]
        ? Get<Prev, Index> extends infer Right extends readonly unknown[]
          ? PascalHelper<N, Prev, [...Curr, [...Left, ...Right]], Acc>
          : never
        : never
      : never;

type Get<T extends readonly unknown[], K extends number> = [K] extends [never]
  ? []
  : `${K}` extends keyof T
    ? T[K]
    : [];

type Tail<T extends readonly unknown[]> = T extends [unknown, ...infer Tail]
  ? Tail
  : [];

type MinusOne<T extends number> = T extends 0
  ? never
  : Tail<NumberToTuple<T>> extends infer T extends readonly unknown[]
    ? T['length']
    : never;

type NumberToTuple<
  T extends number,
  Acc extends readonly unknown[] = [],
> = Acc['length'] extends T ? Acc : NumberToTuple<T, [...Acc, unknown]>;

type ToNumberTuple<T extends ReadonlyArray<readonly unknown[]>> = {
  [Key in keyof T]: T[Key]['length'];
};

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<Pascal<1>, [[1]]>>,
  Expect<Equal<Pascal<3>, [[1], [1, 1], [1, 2, 1]]>>,
  Expect<
    Equal<Pascal<5>, [[1], [1, 1], [1, 2, 1], [1, 3, 3, 1], [1, 4, 6, 4, 1]]>
  >,
  Expect<
    Equal<
      Pascal<7>,
      [
        [1],
        [1, 1],
        [1, 2, 1],
        [1, 3, 3, 1],
        [1, 4, 6, 4, 1],
        [1, 5, 10, 10, 5, 1],
        [1, 6, 15, 20, 15, 6, 1],
      ]
    >
  >,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/30958/answer
  > View solutions: https://tsch.js.org/30958/solutions
  > More Challenges: https://tsch.js.org
*/
