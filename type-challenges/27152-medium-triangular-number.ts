/*
  27152 - Triangular number
  -------
  by null (@aswinsvijay) #medium #tuple #array #math

  ### Question

  Given a number N, find the Nth triangular number, i.e. `1 + 2 + 3 + ... + N`

  > View on GitHub: https://tsch.js.org/27152
*/

/* _____________ Your Code Here _____________ */

// Triangular numbers are calculated by the formula (N + (N * 1)) / 2
type Triangular<N extends number> = IsEven<N> extends infer E
  ? NumberToTuple<N> extends infer T extends readonly unknown[]
    ? // Distribute the division first. If N is even, we'll divide N. If not, we'll divide N + 1.
      Halve<E extends true ? T : [...T, unknown]> extends infer L extends
        readonly unknown[]
      ? // Multiply by the other term. If we divided N by two, multiply by N + 1. If not, multiply by N.
        (E extends true ? [...T, unknown] : T) extends infer R extends
          readonly unknown[]
        ? Repeat<L, R['length']> extends infer Result extends readonly unknown[]
          ? Result['length']
          : never
        : never
      : never
    : never
  : never;

type NumberToTuple<
  N extends number,
  Acc extends readonly unknown[] = [],
> = Acc['length'] extends N ? Acc : NumberToTuple<N, [...Acc, unknown]>;

type EvenDigits = '0' | '2' | '4' | '6' | '8';

type IsEven<N extends number> = `${N}` extends `${string}${EvenDigits}`
  ? true
  : false;

type Repeat<
  T extends readonly unknown[],
  N extends number,
  Acc extends readonly unknown[] = [],
  Counter extends readonly unknown[] = [],
> = Counter['length'] extends N
  ? Acc
  : Repeat<T, N, [...Acc, ...T], [...Counter, unknown]>;

type Halve<
  N extends readonly unknown[],
  Acc extends readonly unknown[] = [],
> = N['length'] extends Acc['length']
  ? N
  : N extends [unknown, ...infer Rest]
  ? Rest['length'] extends Acc['length']
    ? Rest
    : Halve<Rest, [...Acc, unknown]>
  : never;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<Triangular<0>, 0>>,
  Expect<Equal<Triangular<1>, 1>>,
  Expect<Equal<Triangular<3>, 6>>,
  Expect<Equal<Triangular<10>, 55>>,
  Expect<Equal<Triangular<20>, 210>>,
  Expect<Equal<Triangular<55>, 1540>>,
  Expect<Equal<Triangular<100>, 5050>>,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/27152/answer
  > View solutions: https://tsch.js.org/27152/solutions
  > More Challenges: https://tsch.js.org
*/
