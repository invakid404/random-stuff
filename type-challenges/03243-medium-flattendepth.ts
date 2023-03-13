/*
  3243 - FlattenDepth
  -------
  by jiangshan (@jiangshanmeta) #medium #array

  ### Question

  Recursively flatten array up to depth times.

  For example:

  ```typescript
  type a = FlattenDepth<[1, 2, [3, 4], [[[5]]]], 2> // [1, 2, 3, 4, [5]]. flattern 2 times
  type b = FlattenDepth<[1, 2, [3, 4], [[[5]]]]> // [1, 2, 3, 4, [[5]]]. Depth defaults to be 1
  ```

  If the depth is provided, it's guaranteed to be positive integer.

  > View on GitHub: https://tsch.js.org/3243
*/

/* _____________ Your Code Here _____________ */
type Predecessor = ['9', '0', '1', '2', '3', '4', '5', '6', '7', '8'];

type Digit = Predecessor[number];

type SplitLast<
  S extends string,
  Acc extends string = '',
> = S extends `${infer Head}${infer Rest}`
  ? Rest extends ''
    ? [Acc, S]
    : SplitLast<Rest, `${Acc}${Head}`>
  : never;

type StripLeadingZeros<Value extends string> = Value extends '0'
  ? Value
  : Value extends `0${infer Rest}`
  ? StripLeadingZeros<Rest>
  : Value;

type StringToNumber<S extends string> =
  S extends `${infer Value extends number}` ? Value : never;

type MinusOneString<Value extends string> = SplitLast<Value> extends [
  infer Prefix extends string,
  infer Last extends Digit,
]
  ? Predecessor[Last] extends '9'
    ? `${MinusOneString<Prefix>}${Predecessor[Last]}`
    : `${Prefix}${Predecessor[Last]}`
  : never;

type MinusOne<N extends number> = N extends 0
  ? -1
  : StringToNumber<StripLeadingZeros<MinusOneString<`${N}`>>>;

type FlattenDepth<
  T extends readonly unknown[],
  Depth extends number = 1,
> = Depth extends 0
  ? T
  : T extends [infer Head, ...infer Rest]
  ? [
      ...(Head extends readonly unknown[]
        ? FlattenDepth<Head, MinusOne<Depth>>
        : [Head]),
      ...FlattenDepth<Rest, Depth>,
    ]
  : [];

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<FlattenDepth<[]>, []>>,
  Expect<Equal<FlattenDepth<[1, 2, 3, 4]>, [1, 2, 3, 4]>>,
  Expect<Equal<FlattenDepth<[1, [2]]>, [1, 2]>>,
  Expect<Equal<FlattenDepth<[1, 2, [3, 4], [[[5]]]], 2>, [1, 2, 3, 4, [5]]>>,
  Expect<Equal<FlattenDepth<[1, 2, [3, 4], [[[5]]]]>, [1, 2, 3, 4, [[5]]]>>,
  Expect<Equal<FlattenDepth<[1, [2, [3, [4, [5]]]]], 3>, [1, 2, 3, 4, [5]]>>,
  Expect<
    Equal<FlattenDepth<[1, [2, [3, [4, [5]]]]], 19260817>, [1, 2, 3, 4, 5]>
  >,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/3243/answer
  > View solutions: https://tsch.js.org/3243/solutions
  > More Challenges: https://tsch.js.org
*/
