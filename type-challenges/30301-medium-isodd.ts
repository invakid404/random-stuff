/*
  30301 - IsOdd
  -------
  by jiangshan (@jiangshanmeta) #medium #string

  ### Question

  return true is a number is odd

  > View on GitHub: https://tsch.js.org/30301
*/

/* _____________ Your Code Here _____________ */
type OddDigit = '1' | '3' | '5' | '7' | '9';

type IsOdd<T extends number> = `${T}` extends `${string}${OddDigit}`
  ? true
  : false;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<IsOdd<2023>, true>>,
  Expect<Equal<IsOdd<1453>, true>>,
  Expect<Equal<IsOdd<1926>, false>>,
  Expect<Equal<IsOdd<number>, false>>,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/30301/answer
  > View solutions: https://tsch.js.org/30301/solutions
  > More Challenges: https://tsch.js.org
*/
