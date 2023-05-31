/*
  9155 - ValidDate
  -------
  by ch3cknull (@ch3cknull) #hard

  ### Question

  Implement a type `ValidDate`, which takes an input type T and returns whether T is a valid date.

  **Leap year is not considered**

  Good Luck!

  ```ts
  ValidDate<'0102'> // true
  ValidDate<'0131'> // true
  ValidDate<'1231'> // true
  ValidDate<'0229'> // false
  ValidDate<'0100'> // false
  ValidDate<'0132'> // false
  ValidDate<'1301'> // false
  ```

  > View on GitHub: https://tsch.js.org/9155
*/

/* _____________ Your Code Here _____________ */
type LessThanOrEqual<
  Left extends number,
  Right extends number,
  Acc extends unknown[] = [],
> = Acc['length'] extends Left
  ? true
  : Acc['length'] extends Right
  ? false
  : LessThanOrEqual<Left, Right, [...Acc, unknown]>;

type MonthDays = {
  '01': 31;
  '02': 28;
  '03': 31;
  '04': 30;
  '05': 31;
  '06': 30;
  '07': 31;
  '08': 31;
  '09': 30;
  '10': 31;
  '11': 30;
  '12': 31;
};

type Expand<T> = T extends unknown ? T : never;

type Month = Expand<keyof MonthDays>;

type Join<T extends readonly string[]> = T extends [
  infer Head extends string,
  ...infer Rest extends readonly string[],
]
  ? `${Head}${Join<Rest>}`
  : '';

type SplitAt<
  T extends string,
  N extends number,
  Acc extends string[] = [],
> = Acc['length'] extends N
  ? [Join<Acc>, T]
  : T extends `${infer Head}${infer Rest}`
  ? SplitAt<Rest, N, [...Acc, Head]>
  : [Join<Acc>, T];

type TrimLeadingZeros<T extends string> = T extends `0${infer Rest}`
  ? TrimLeadingZeros<Rest>
  : T;

type ValidDate<T extends string> = 4 extends T['length']
  ? SplitAt<T, 2> extends [infer M, infer D extends string]
    ? M extends Month
      ? TrimLeadingZeros<D> extends `${infer DN extends number}`
        ? LessThanOrEqual<DN, MonthDays[M]> extends true
          ? true
          : false
        : false
      : false
    : never
  : false;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<ValidDate<'0102'>, true>>,
  Expect<Equal<ValidDate<'0131'>, true>>,
  Expect<Equal<ValidDate<'1231'>, true>>,
  Expect<Equal<ValidDate<'0229'>, false>>,
  Expect<Equal<ValidDate<'0100'>, false>>,
  Expect<Equal<ValidDate<'0132'>, false>>,
  Expect<Equal<ValidDate<'1301'>, false>>,
  Expect<Equal<ValidDate<'0123'>, true>>,
  Expect<Equal<ValidDate<'01234'>, false>>,
  Expect<Equal<ValidDate<''>, false>>,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/9155/answer
  > View solutions: https://tsch.js.org/9155/solutions
  > More Challenges: https://tsch.js.org
*/
