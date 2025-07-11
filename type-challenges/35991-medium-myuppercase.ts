/*
  35991 - MyUppercase
  -------
  by NaNix (@fusheng33) #medium

  ### Question

  Implement `Uppercase<T>`, convert all letter to uppercase

  > View on GitHub: https://tsch.js.org/35991
*/

/* _____________ Your Code Here _____________ */
type MyUppercase<
  T extends string,
  Acc extends string = '',
> = T extends `${infer Head}${infer Rest}`
  ? MyUppercase<
      Rest,
      Head extends keyof ToUpper ? `${Acc}${ToUpper[Head]}` : `${Acc}${Head}`
    >
  : Acc;

type ToUpper = {
  a: 'A';
  b: 'B';
  c: 'C';
  d: 'D';
  e: 'E';
  f: 'F';
  g: 'G';
  h: 'H';
  i: 'I';
  j: 'J';
  k: 'K';
  l: 'L';
  m: 'M';
  n: 'N';
  o: 'O';
  p: 'P';
  q: 'Q';
  r: 'R';
  s: 'S';
  t: 'T';
  u: 'U';
  v: 'V';
  w: 'W';
  x: 'X';
  y: 'Y';
  z: 'Z';
};

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<MyUppercase<'a'>, 'A'>>,
  Expect<Equal<MyUppercase<'Z'>, 'Z'>>,
  Expect<
    Equal<MyUppercase<'A z h yy 😃cda\n\t  a   '>, 'A Z H YY 😃CDA\n\t  A   '>
  >,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/35991/answer
  > View solutions: https://tsch.js.org/35991/solutions
  > More Challenges: https://tsch.js.org
*/
