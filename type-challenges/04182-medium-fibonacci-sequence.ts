/*
  4182 - Fibonacci Sequence
  -------
  by windliang (@wind-liang) #medium

  ### Question

  Implement a generic `Fibonacci<T>` that takes a number `T` and returns its corresponding [Fibonacci number](https://en.wikipedia.org/wiki/Fibonacci_number).

  The sequence starts:
  1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, ...

  For example
  ```ts
  type Result1 = Fibonacci<3> // 2
  type Result2 = Fibonacci<8> // 21
  ```

  > View on GitHub: https://tsch.js.org/4182
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

type FibonacciRec<T extends number> = T extends 0
  ? []
  : T extends 1
  ? [unknown]
  : [...FibonacciRec<MinusOne<MinusOne<T>>>, ...FibonacciRec<MinusOne<T>>];

type Fibonacci<T extends number> = FibonacciRec<T>['length'];

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<Fibonacci<1>, 1>>,
  Expect<Equal<Fibonacci<2>, 1>>,
  Expect<Equal<Fibonacci<3>, 2>>,
  Expect<Equal<Fibonacci<8>, 21>>,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/4182/answer
  > View solutions: https://tsch.js.org/4182/solutions
  > More Challenges: https://tsch.js.org
*/
