/*
  300 - String to Number
  -------
  by Pig Fang (@g-plane) #hard #template-literal
  
  ### Question
  
  Convert a string literal to a number, which behaves like `Number.parseInt`.
  
  > View on GitHub: https://tsch.js.org/300
*/

/* _____________ Your Code Here _____________ */

type MultiplyBy10<Array extends readonly any[]> = [
  ...Array,
  ...Array,
  ...Array,
  ...Array,
  ...Array,
  ...Array,
  ...Array,
  ...Array,
  ...Array,
  ...Array,
];

type DigitToArray = {
  '0': [];
  '1': [any];
  '2': [any, any];
  '3': [any, any, any];
  '4': [any, any, any, any];
  '5': [any, any, any, any, any];
  '6': [any, any, any, any, any, any];
  '7': [any, any, any, any, any, any, any];
  '8': [any, any, any, any, any, any, any, any];
  '9': [any, any, any, any, any, any, any, any, any];
};

type Digit = keyof DigitToArray;

type ToNumber<
  S extends string,
  Accumulator extends readonly any[] = [],
> = S extends ''
  ? Accumulator['length']
  : S extends `${infer Head extends Digit}${infer Rest}`
  ? ToNumber<Rest, [...MultiplyBy10<Accumulator>, ...DigitToArray[Head]]>
  : never;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<ToNumber<'0'>, 0>>,
  Expect<Equal<ToNumber<'5'>, 5>>,
  Expect<Equal<ToNumber<'12'>, 12>>,
  Expect<Equal<ToNumber<'27'>, 27>>,
  Expect<Equal<ToNumber<'18@7_$%'>, never>>,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/300/answer
  > View solutions: https://tsch.js.org/300/solutions
  > More Challenges: https://tsch.js.org
*/
