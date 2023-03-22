/*
  4499 - Chunk
  -------
  by キリサメ qianxi (@qianxi0410) #medium #tuple

  ### Question

  Do you know `lodash`? `Chunk` is a very useful function in it, now let's implement it.
  `Chunk<T, N>` accepts two required type parameters, the `T` must be a `tuple`, and the `N` must be an `integer >=1`

  ```ts
  type exp1 = Chunk<[1, 2, 3], 2> // expected to be [[1, 2], [3]]
  type exp2 = Chunk<[1, 2, 3], 4> // expected to be [[1, 2, 3]]
  type exp3 = Chunk<[1, 2, 3], 1> // expected to be [[1], [2], [3]]
  ```

  > View on GitHub: https://tsch.js.org/4499
*/

/* _____________ Your Code Here _____________ */
type NextChunk<
  T extends readonly unknown[],
  Size extends number,
  Acc extends readonly unknown[] = [],
> = Acc['length'] extends Size
  ? [Acc, T]
  : T extends [infer Head, ...infer Rest]
  ? NextChunk<Rest, Size, [...Acc, Head]>
  : [Acc, T];

type ChunkHelper<
  T extends readonly unknown[],
  Size extends number,
  Acc extends readonly unknown[] = [],
> = T['length'] extends 0
  ? Acc
  : NextChunk<T, Size> extends [
      infer Chunk,
      infer Rest extends readonly unknown[],
    ]
  ? ChunkHelper<Rest, Size, [...Acc, Chunk]>
  : never;

type Chunk<T extends readonly unknown[], Size extends number> = ChunkHelper<
  T,
  Size
>;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<Chunk<[], 1>, []>>,
  Expect<Equal<Chunk<[1, 2, 3], 1>, [[1], [2], [3]]>>,
  Expect<Equal<Chunk<[1, 2, 3], 2>, [[1, 2], [3]]>>,
  Expect<Equal<Chunk<[1, 2, 3, 4], 2>, [[1, 2], [3, 4]]>>,
  Expect<Equal<Chunk<[1, 2, 3, 4], 5>, [[1, 2, 3, 4]]>>,
  Expect<Equal<Chunk<[1, true, 2, false], 2>, [[1, true], [2, false]]>>,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/4499/answer
  > View solutions: https://tsch.js.org/4499/solutions
  > More Challenges: https://tsch.js.org
*/
