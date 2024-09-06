/*
  31824 - Length of String 3
  -------
  by Eosellmay Li (@E0SelmY4V) #hard

  ### Question

  Implement a type `LengthOfString<S>` just like `Array#length`:

  Differing to two previous challenges about strings' length, this times the type must support strings about $10^6$ characters long, which makes it more challenging.

  > View on GitHub: https://tsch.js.org/31824
*/

/* _____________ Your Code Here _____________ */
type StringToNumber<S> = S extends `0${infer X}`
  ? StringToNumber<X>
  : S extends `${infer N extends number}`
    ? N
    : 0;

type Q1 = string;
type Q10 = `${Q1}${Q1}${Q1}${Q1}${Q1}${Q1}${Q1}${Q1}${Q1}${Q1 & {}}`;
type Q100 = `${Q10}${Q10}${Q10}${Q10}${Q10}${Q10}${Q10}${Q10}${Q10}${Q10}`;
type Q1000 =
  `${Q100}${Q100}${Q100}${Q100}${Q100}${Q100}${Q100}${Q100}${Q100}${Q100}`;
type Q10k =
  `${Q1000}${Q1000}${Q1000}${Q1000}${Q1000}${Q1000}${Q1000}${Q1000}${Q1000}${Q1000}`;
type Q100k =
  `${Q10k}${Q10k}${Q10k}${Q10k}${Q10k}${Q10k}${Q10k}${Q10k}${Q10k}${Q10k}`;

type LengthOfStringHelper<
  S,
  Q extends string,
  Acc extends readonly unknown[] = [],
> = S extends `${Q}${infer T}`
  ? LengthOfStringHelper<T, Q, [...Acc, unknown]>
  : [Acc['length'], S];

type LengthOfString<S extends string> = LengthOfStringHelper<S, Q100k> extends [
  infer A extends number,
  infer S1,
]
  ? LengthOfStringHelper<S1, Q10k> extends [infer B extends number, infer S2]
    ? LengthOfStringHelper<S2, Q1000> extends [infer C extends number, infer S3]
      ? LengthOfStringHelper<S3, Q100> extends [
          infer D extends number,
          infer S4,
        ]
        ? LengthOfStringHelper<S4, Q10> extends [
            infer E extends number,
            infer S5,
          ]
          ? LengthOfStringHelper<S5, Q1> extends [
              infer F extends number,
              string,
            ]
            ? StringToNumber<`${A}${B}${C}${D}${E}${F}`>
            : 0
          : 0
        : 0
      : 0
    : 0
  : 0;
/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type Pred = [never, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9];

type Digit = Pred[number];

type Repeat<
  T extends string,
  N extends Digit,
  Acc extends string = '',
> = N extends 0 ? Acc : Repeat<T, Pred[N], `${Acc}${T}`>;

type T1 = 'k';
type T10 = `${T1}${T1}${T1}${T1}${T1}${T1}${T1}${T1}${T1}${T1}`;
type T100 = `${T10}${T10}${T10}${T10}${T10}${T10}${T10}${T10}${T10}${T10}`;
type T1K =
  `${T100}${T100}${T100}${T100}${T100}${T100}${T100}${T100}${T100}${T100}`;
type T10K = `${T1K}${T1K}${T1K}${T1K}${T1K}${T1K}${T1K}${T1K}${T1K}${T1K}`;
type T100K =
  `${T10K}${T10K}${T10K}${T10K}${T10K}${T10K}${T10K}${T10K}${T10K}${T10K}`;
type T1M =
  `${T100K}${T100K}${T100K}${T100K}${T100K}${T100K}${T100K}${T100K}${T100K}${T100K}`;

type Digits<
  T extends string,
  Acc extends readonly Digit[] = [],
> = T extends `${infer Head extends Digit}${infer Rest}`
  ? Digits<Rest, [...Acc, Head]>
  : Acc;

type Gened<T extends string> = Digits<T> extends [
  infer C1M extends Digit,
  infer C100K extends Digit,
  infer C10K extends Digit,
  infer C1K extends Digit,
  infer C100 extends Digit,
  infer C10 extends Digit,
  infer C1 extends Digit,
]
  ? Repeat<T1M, C1M> extends infer R1M extends string
    ? Repeat<T100K, C100K> extends infer R100K extends string
      ? Repeat<T10K, C10K> extends infer R10K extends string
        ? Repeat<T1K, C1K> extends infer R1K extends string
          ? Repeat<T100, C100> extends infer R100 extends string
            ? Repeat<T10, C10> extends infer R10 extends string
              ? Repeat<T1, C1> extends infer R1 extends string
                ? `${R1M}${R100K}${R10K}${R1K}${R100}${R10}${R1}`
                : never
              : never
            : never
          : never
        : never
      : never
    : never
  : never;

type cases = [
  Expect<Equal<LengthOfString<Gened<'0000000'>>, 0>>,
  Expect<Equal<LengthOfString<Gened<'0000001'>>, 1>>,
  Expect<Equal<LengthOfString<Gened<'0000002'>>, 2>>,
  Expect<Equal<LengthOfString<Gened<'0000003'>>, 3>>,
  Expect<Equal<LengthOfString<Gened<'0000004'>>, 4>>,
  Expect<Equal<LengthOfString<Gened<'0000005'>>, 5>>,
  Expect<Equal<LengthOfString<Gened<'0000055'>>, 55>>,
  Expect<Equal<LengthOfString<Gened<'0000555'>>, 555>>,
  Expect<Equal<LengthOfString<Gened<'0005555'>>, 5555>>,
  Expect<Equal<LengthOfString<Gened<'0055555'>>, 55555>>,
  Expect<Equal<LengthOfString<Gened<'8464592'>>, 8464592>>,
  Expect<Equal<LengthOfString<Gened<'1373690'>>, 1373690>>,
  Expect<Equal<LengthOfString<Gened<'1707793'>>, 1707793>>,
  Expect<Equal<LengthOfString<Gened<'0196268'>>, 196268>>,
  Expect<Equal<LengthOfString<Gened<'6646734'>>, 6646734>>,
  Expect<Equal<LengthOfString<Gened<'0538159'>>, 538159>>,
  Expect<Equal<LengthOfString<Gened<'0058901'>>, 58901>>,
  Expect<Equal<LengthOfString<Gened<'8414001'>>, 8414001>>,
  Expect<Equal<LengthOfString<Gened<'1740697'>>, 1740697>>,
  Expect<Equal<LengthOfString<Gened<'2281441'>>, 2281441>>,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/31824/answer
  > View solutions: https://tsch.js.org/31824/solutions
  > More Challenges: https://tsch.js.org
*/
