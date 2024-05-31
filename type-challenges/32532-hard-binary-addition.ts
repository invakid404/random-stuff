/*
  32532 - Binary Addition
  -------
  by Finley Garton (@finleygn) #hard #recursion #array

  ### Question

  Implement `BinaryAdd` to add two binary numbers together. The numbers should not be translated out of binary at any point.

  Note the two inputs will always have the same length.

  > View on GitHub: https://tsch.js.org/32532
*/

/* _____________ Your Code Here _____________ */
type Bit = 1 | 0;

type Init<T extends readonly unknown[]> = T extends [...infer Init, unknown]
  ? Init
  : [];

type LastBit<Bits extends Bit[]> = Bits extends [...Bit[], infer LastBit]
  ? LastBit
  : 0;

type BothEmpty<
  Left extends readonly unknown[],
  Right extends readonly unknown[],
> = Left extends [] ? (Right extends [] ? true : false) : false;

type BitSum = {
  1: {
    1: [0, true];
    0: [1, false];
  };
  0: {
    1: [1, false];
    0: [0, false];
  };
};

type SumBitsWithoutCarry<
  Left extends Bit,
  Right extends Bit,
> = BitSum[Left][Right];

type SumBitsWithCarry<
  Left extends Bit,
  Right extends Bit,
> = SumBitsWithoutCarry<Left, Right> extends [
  infer Result extends Bit,
  infer Carry extends boolean,
]
  ? SumBitsWithoutCarry<Result, 1> extends [infer ResultWithCarry, boolean]
    ? [ResultWithCarry, Carry]
    : never
  : never;

type SumBits<
  Left extends Bit,
  Right extends Bit,
  Carry extends boolean,
> = Carry extends true
  ? SumBitsWithCarry<Left, Right>
  : SumBitsWithoutCarry<Left, Right>;

type BinaryAddHelper<
  Left extends Bit[],
  Right extends Bit[],
  Carry extends boolean = false,
> = BothEmpty<Left, Right> extends true
  ? Carry extends true
    ? [1]
    : []
  : SumBits<LastBit<Left>, LastBit<Right>, Carry> extends [
        infer Result extends Bit,
        infer Carry extends boolean,
      ]
    ? [...BinaryAddHelper<Init<Left>, Init<Right>, Carry>, Result]
    : never;

type BinaryAdd<A extends Bit[], B extends Bit[]> = BinaryAddHelper<A, B>;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<BinaryAdd<[1], [1]>, [1, 0]>>,
  Expect<Equal<BinaryAdd<[0], [1]>, [1]>>,
  Expect<Equal<BinaryAdd<[1, 1, 0], [0, 0, 1]>, [1, 1, 1]>>,
  Expect<
    Equal<
      BinaryAdd<
        [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
        [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
      >,
      [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0]
    >
  >,
  Expect<
    Equal<
      BinaryAdd<[1, 0, 1, 0, 1, 1, 1, 0], [1, 0, 0, 0, 1, 1, 0, 0]>,
      [1, 0, 0, 1, 1, 1, 0, 1, 0]
    >
  >,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/32532/answer
  > View solutions: https://tsch.js.org/32532/solutions
  > More Challenges: https://tsch.js.org
*/
