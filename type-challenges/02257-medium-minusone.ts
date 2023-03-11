/*
  2257 - MinusOne
  -------
  by Mustafo Faiz (@fayzzzm) #medium #math

  ### Question

  Given a number (always positive) as a type. Your type should return the number decreased by one.

  For example:

  ```ts
  type Zero = MinusOne<1> // 0
  type FiftyFour = MinusOne<55> // 54
  ```

  > View on GitHub: https://tsch.js.org/2257
*/

/* _____________ Your Code Here _____________ */
type Successor = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'];
type Predecessor = ['9', '0', '1', '2', '3', '4', '5', '6', '7', '8'];

type Digit = Successor[number];

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

type MinusOneString<Value extends string> = SplitLast<Value> extends [
  infer Prefix extends string,
  infer Last extends Digit,
]
  ? Predecessor[Last] extends '9'
    ? `${MinusOneString<Prefix>}${Predecessor[Last]}`
    : `${Prefix}${Predecessor[Last]}`
  : never;

type AddOneString<Value extends string> = Value extends ''
  ? 1
  : SplitLast<Value> extends [
      infer Prefix extends string,
      infer Last extends Digit,
    ]
  ? Successor[Last] extends '0'
    ? `${AddOneString<Prefix>}${Successor[Last]}`
    : `${Prefix}${Successor[Last]}`
  : never;

type StringToNumber<S extends string> =
  S extends `${infer Value extends number}` ? Value : never;

type Abs<N extends number> = `${N}` extends infer Value extends string
  ? Value extends '0'
    ? [Value, false]
    : Value extends `-${infer Abs}`
    ? [Abs, false]
    : [Value, true]
  : never;

type MinusOne<Value extends number> = Abs<Value> extends [
  infer Abs extends string,
  infer IsPositive extends boolean,
]
  ? StringToNumber<
      IsPositive extends true
        ? StripLeadingZeros<MinusOneString<Abs>>
        : `-${AddOneString<Abs>}`
    >
  : never;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<MinusOne<1>, 0>>,
  Expect<Equal<MinusOne<55>, 54>>,
  Expect<Equal<MinusOne<3>, 2>>,
  Expect<Equal<MinusOne<100>, 99>>,
  Expect<Equal<MinusOne<1101>, 1100>>,
  Expect<Equal<MinusOne<0>, -1>>,
  Expect<Equal<MinusOne<-99>, -100>>,
  Expect<Equal<MinusOne<9_007_199_254_740_992>, 9_007_199_254_740_991>>,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/2257/answer
  > View solutions: https://tsch.js.org/2257/solutions
  > More Challenges: https://tsch.js.org
*/
