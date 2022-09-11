/*
  476 - Sum
  -------
  by null (@uid11) #extreme #math #template-literal
  
  ### Question
  
  Implement a type `Sum<A, B>` that summing two non-negative integers and returns the sum as a string. Numbers can be specified as a string, number, or bigint.
  
  For example,
  
  ```ts
  type T0 = Sum<2, 3> // '5'
  type T1 = Sum<'13', '21'> // '34'
  type T2 = Sum<'328', 7> // '335'
  type T3 = Sum<1_000_000_000_000n, '123'> // '1000000000123'
  ```
  
  > View on GitHub: https://tsch.js.org/476
*/

/* _____________ Your Code Here _____________ */
type Predecessor = {
  0: never;
  1: 0;
  2: 1;
  3: 2;
  4: 3;
  5: 4;
  6: 5;
  7: 6;
  8: 7;
  9: 8;
};

type Successor = {
  0: 1;
  1: 2;
  2: 3;
  3: 4;
  4: 5;
  5: 6;
  6: 7;
  7: 8;
  8: 9;
  // Invariant: the only digit with a successor of 0 is 9, meaning we don't
  // need any extra information to determine whether we need to carry. Getting
  // a 0 while adding two digits is enough.
  9: 0;
};

type Digit = keyof Successor;

type StringToDigit = { [key in Digit as `${key}`]: key };

// Compute the sum of two digits by recursively incrementing `Left` and
// decrementing `Right`, keeping track of the carry.
type SumDigitsWithoutCarry<
  Left extends Digit,
  Right extends Digit,
  Carry extends boolean = false,
> = Right extends 0
  ? [Left, Carry]
  : Successor[Left] extends infer LeftSuccessor extends Digit
  ? SumDigitsWithoutCarry<
      LeftSuccessor,
      Predecessor[Right],
      LeftSuccessor extends 0 ? true : Carry
    >
  : never;

// Compute the sum of two digits, applying a carry at the end.
type SumDigitsWithCarry<
  Left extends Digit,
  Right extends Digit,
> = SumDigitsWithoutCarry<Left, Right> extends [
  infer Result extends Digit,
  infer Carry extends boolean,
]
  ? Successor[Result] extends infer ResultSuccessor extends Digit
    ? [
        ResultSuccessor,
        // We have a carry if either the actual sum or the final increment
        // resulted in a carry.
        Carry extends true ? Carry : ResultSuccessor extends 0 ? true : false,
      ]
    : never
  : never;

type SumDigits<
  Left extends Digit,
  Right extends Digit,
  Carry extends boolean,
> = Carry extends true
  ? SumDigitsWithCarry<Left, Right>
  : SumDigitsWithoutCarry<Left, Right>;

type ToDigits<Value extends string> =
  Value extends `${infer Head extends keyof StringToDigit}${infer Rest}`
    ? [StringToDigit[Head], ...ToDigits<Rest>]
    : [];

type BothEmpty<
  Left extends readonly any[],
  Right extends readonly any[],
> = Left extends [] ? (Right extends [] ? true : false) : false;

type Init<Array extends readonly any[]> = Array extends [...infer Init, any]
  ? Init
  : [];

// Get the last digit of a digit slice, returning 0 if empty.
//
// This type allows us to effectively "pad" both numbers to be the same length
// without going through all the trouble.
type LastDigit<Digits extends readonly Digit[]> = Digits extends [
  ...Digit[],
  infer LastDigit,
]
  ? LastDigit
  : 0;

type SumDigitSlices<
  Left extends readonly Digit[],
  Right extends readonly Digit[],
  Carry extends boolean = false,
> = BothEmpty<Left, Right> extends true
  ? Carry extends true
    ? [1] // If we still have a carry, return a leading one.
    : []
  : SumDigits<LastDigit<Left>, LastDigit<Right>, Carry> extends [
      infer Result,
      infer Carry extends boolean,
    ]
  ? [...SumDigitSlices<Init<Left>, Init<Right>, Carry>, Result]
  : never;

type DigitsToString<Digits extends readonly Digit[]> = Digits extends [
  infer Head extends Digit,
  ...infer Tail extends readonly Digit[],
]
  ? `${Head}${DigitsToString<Tail>}`
  : '';

type Sum<
  Left extends number | bigint | string,
  Right extends number | bigint | string,
> = SumDigitSlices<
  ToDigits<`${Left}`>,
  ToDigits<`${Right}`>
> extends infer Result extends readonly Digit[]
  ? DigitsToString<Result>
  : never;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<Sum<2, 3>, '5'>>,
  Expect<Equal<Sum<'13', '21'>, '34'>>,
  Expect<Equal<Sum<'328', 7>, '335'>>,
  Expect<Equal<Sum<1_000_000_000_000n, '123'>, '1000000000123'>>,
  Expect<Equal<Sum<9999, 1>, '10000'>>,
  Expect<Equal<Sum<4325234, '39532'>, '4364766'>>,
  Expect<Equal<Sum<728, 0>, '728'>>,
  Expect<Equal<Sum<'0', 213>, '213'>>,
  Expect<Equal<Sum<0, '0'>, '0'>>,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/476/answer
  > View solutions: https://tsch.js.org/476/solutions
  > More Challenges: https://tsch.js.org
*/
