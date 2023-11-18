/*
  30575 - BitwiseXOR
  -------
  by jiangshan (@jiangshanmeta) #hard

  ### Question

  Implement ```BitwiseXOR<S1,S2>``` which takes two binary string literal type and returns a binary string that reprents the bitwise XOR of S1 and S2

  For example:

  ```typescript
  BitwiseXOR<'0','1'> // expect '1'
  BitwiseXOR<'1','1'> // expect '0'
  BitwiseXOR<'10','1'>  // expect '11'
  ```

  > View on GitHub: https://tsch.js.org/30575
*/

/* _____________ Your Code Here _____________ */
type BitwiseXOR<S1 extends string, S2 extends string> = Reverse<
  BitwiseXORHelper<Reverse<S1>, Reverse<S2>>
>;

type BitwiseXORHelper<S1 extends string, S2 extends string> = BothEmpty<
  S1,
  S2
> extends true
  ? ''
  : `${FirstBit<S1> extends FirstBit<S2> ? '0' : '1'}${BitwiseXORHelper<
      Rest<S1>,
      Rest<S2>
    >}`;

type BothEmpty<S1 extends string, S2 extends string> = S1 extends ''
  ? S2 extends ''
    ? true
    : false
  : false;

type FirstBit<S extends string> = S extends `${infer Head}${string}`
  ? Head
  : '0';

type Rest<S extends string> = S extends `${string}${infer Rest}` ? Rest : '';

type Reverse<
  S extends string,
  Acc extends string = '',
> = S extends `${infer Head}${infer Rest}`
  ? Reverse<Rest, `${Head}${Acc}`>
  : Acc;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<BitwiseXOR<'0', '1'>, '1'>>,
  Expect<Equal<BitwiseXOR<'1', '1'>, '0'>>,
  Expect<Equal<BitwiseXOR<'10', '1'>, '11'>>,
  Expect<Equal<BitwiseXOR<'110', '1'>, '111'>>,
  Expect<Equal<BitwiseXOR<'101', '11'>, '110'>>,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/30575/answer
  > View solutions: https://tsch.js.org/30575/solutions
  > More Challenges: https://tsch.js.org
*/
