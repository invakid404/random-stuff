/*
  27133 - Square
  -------
  by null (@aswinsvijay) #medium #tuple #array #math

  ### Question

  Given a number, your type should return its square.

  > View on GitHub: https://tsch.js.org/27133
*/

/* _____________ Your Code Here _____________ */

// This solution reuses `Multiply<Left, Right>`: an extreme challenge, which
// leads me to believe that this isn't the intended solution, but we ball.

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

type ToDigits<Value extends string> = Value extends `${infer Head extends
  keyof StringToDigit}${infer Rest}`
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

// A matrix, containing the result of the multiplication of any two digits,
// represented as a digit slice.
type MultiplicationTable = { [key in Digit]: MultiplicationRow<key> };

// Computes the row of the multiplication table for a given digit by
// repeatedly adding the digit to the previous result.
type MultiplicationRow<
  Value extends Digit,
  Accumulator extends readonly (readonly Digit[])[] = [[0]],
> = Accumulator['length'] extends 10
  ? Accumulator
  : Accumulator extends [...any[], infer Last extends readonly Digit[]]
  ? SumDigitSlices<Last, [Value]> extends infer Result extends readonly Digit[]
    ? MultiplicationRow<Value, [...Accumulator, Result]>
    : never
  : never;

// Multiplies a value with a single digit factor.
type MultiplyWithDigit<
  Value extends readonly Digit[],
  Factor extends Digit,
  Result extends readonly Digit[] = [],
  Carry extends Digit = 0,
> = Value extends []
  ? Carry extends 0
    ? Result
    : [Carry, ...Result] // If we still have a carry, append it to the front.
  : SumDigitSlices<
      // Multiply the last digit with the factor...
      MultiplicationTable[LastDigit<Value>][Factor],
      // ...and add the carry to it.
      [Carry]
    > extends infer Current extends readonly Digit[]
  ? // If we get a two-digit result, the first digit is the carry.
    Current extends [infer Carry extends Digit, infer Out extends Digit]
    ? MultiplyWithDigit<Init<Value>, Factor, [Out, ...Result], Carry>
    : MultiplyWithDigit<Init<Value>, Factor, [...Current, ...Result], 0>
  : never;

// Multiplies two digit slices, applying the method taught in school:
// multiply the left number by each of the digits of the right one in reverse
// order, shifting every next result by one digit to the left, then add up all
// intermediate values.
type MultiplyDigitSlices<
  Left extends readonly Digit[],
  Right extends readonly Digit[],
  // Zeros to shift the result of the current single-digit multiplication by.
  // Essentially acts like multiplying by a factor of 10.
  Padding extends readonly Digit[] = [],
> = Right extends []
  ? [0]
  : SumDigitSlices<
      [...MultiplyWithDigit<Left, LastDigit<Right>>, ...Padding],
      MultiplyDigitSlices<Left, Init<Right>, [...Padding, 0]>
    >;

// Removes all leading zeros from a digit slice.
type TrimLeadingZeros<Value extends readonly Digit[]> = Value extends [0]
  ? Value
  : Value extends [0, ...infer Rest extends readonly Digit[]]
  ? TrimLeadingZeros<Rest>
  : Value;

type Multiply<
  Left extends string | number | bigint,
  Right extends string | number | bigint,
> = MultiplyDigitSlices<
  ToDigits<`${Left}`>,
  ToDigits<`${Right}`>
> extends infer Result extends readonly Digit[]
  ? DigitsToString<TrimLeadingZeros<Result>>
  : never;

type Square<N extends number> = Abs<N> extends infer A extends number
  ? Multiply<A, A> extends `${infer Res extends number}`
    ? Res
    : never
  : never;

type Abs<T extends number> = `${T}` extends `-${infer Abs extends number}`
  ? Abs
  : T;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<Square<0>, 0>>,
  Expect<Equal<Square<1>, 1>>,
  Expect<Equal<Square<3>, 9>>,
  Expect<Equal<Square<20>, 400>>,
  Expect<Equal<Square<100>, 10000>>,

  // Negative numbers
  Expect<Equal<Square<-2>, 4>>,
  Expect<Equal<Square<-5>, 25>>,
  Expect<Equal<Square<-31>, 961>>,
  Expect<Equal<Square<-50>, 2500>>,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/27133/answer
  > View solutions: https://tsch.js.org/27133/solutions
  > More Challenges: https://tsch.js.org
*/
