/*
  4425 - Greater Than
  -------
  by ch3cknull (@ch3cknull) #medium #array

  ### Question

  In This Challenge, You should implement a type `GreaterThan<T, U>` like `T > U`

  Negative numbers do not need to be considered.

  For example

  ```ts
  GreaterThan<2, 1> //should be true
  GreaterThan<1, 1> //should be false
  GreaterThan<10, 100> //should be false
  GreaterThan<111, 11> //should be true
  ```

  Good Luck!

  > View on GitHub: https://tsch.js.org/4425
*/

/* _____________ Your Code Here _____________ */
type Digit = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9;

type DigitToSlice = {
  0: [];
  1: [...DigitToSlice[0], unknown];
  2: [...DigitToSlice[1], unknown];
  3: [...DigitToSlice[2], unknown];
  4: [...DigitToSlice[3], unknown];
  5: [...DigitToSlice[4], unknown];
  6: [...DigitToSlice[5], unknown];
  7: [...DigitToSlice[6], unknown];
  8: [...DigitToSlice[7], unknown];
  9: [...DigitToSlice[8], unknown];
};

type Tail<Slice extends readonly any[]> = Slice extends [any, ...infer Tail]
  ? Tail
  : [];

type IsLongerThanHelper<
  Left extends readonly unknown[],
  Right extends readonly unknown[],
  Acc extends readonly unknown[] = [],
> = Acc['length'] extends Right['length']
  ? true
  : Left extends []
  ? false
  : IsLongerThanHelper<Tail<Left>, Right, [...Acc, Left[0]]>;

type IsLongerThan<
  Left extends readonly unknown[],
  Right extends readonly unknown[],
> = Left['length'] extends Right['length']
  ? false
  : IsLongerThanHelper<Left, Right>;

type StringToDigits<T extends string> =
  T extends `${infer Head extends Digit}${infer Rest}`
    ? [Head, ...StringToDigits<Rest>]
    : [];

type NumberToDigits<T extends number> = StringToDigits<`${T}`>;

type IsDigitGreaterThan<Left extends Digit, Right extends Digit> = IsLongerThan<
  DigitToSlice[Left],
  DigitToSlice[Right]
>;

type GreaterThanDigitsHelper<
  Left extends readonly Digit[],
  Right extends readonly Digit[],
> = Left extends []
  ? false
  : Left extends [
      infer LeftHead extends Digit,
      ...infer LeftRest extends readonly Digit[],
    ]
  ? Right extends [
      infer RightHead extends Digit,
      ...infer RightRest extends readonly Digit[],
    ]
    ? IsDigitGreaterThan<LeftHead, RightHead> extends true
      ? true
      : GreaterThanDigitsHelper<LeftRest, RightRest>
    : never
  : never;

type GreaterThanDigits<
  Left extends readonly Digit[],
  Right extends readonly Digit[],
> = IsLongerThan<Left, Right> extends true
  ? true
  : Left['length'] extends Right['length']
  ? GreaterThanDigitsHelper<Left, Right>
  : false;

type GreaterThan<Left extends number, Right extends number> = Left extends Right
  ? false
  : GreaterThanDigits<NumberToDigits<Left>, NumberToDigits<Right>>;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<GreaterThan<1, 0>, true>>,
  Expect<Equal<GreaterThan<5, 4>, true>>,
  Expect<Equal<GreaterThan<4, 5>, false>>,
  Expect<Equal<GreaterThan<0, 0>, false>>,
  Expect<Equal<GreaterThan<10, 9>, true>>,
  Expect<Equal<GreaterThan<20, 20>, false>>,
  Expect<Equal<GreaterThan<10, 100>, false>>,
  Expect<Equal<GreaterThan<111, 11>, true>>,
  Expect<Equal<GreaterThan<1234567891011, 1234567891010>, true>>,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/4425/answer
  > View solutions: https://tsch.js.org/4425/solutions
  > More Challenges: https://tsch.js.org
*/
