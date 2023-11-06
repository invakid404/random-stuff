/*
  14188 - Run-length encoding
  -------
  by Hen Hedymdeith (@alfaproxima) #hard

  ### Question

  Given a `string` sequence of a letters f.e. `AAABCCXXXXXXY`. Return run-length encoded string `3AB2C6XY`.
  Also make a decoder for that string.

  > View on GitHub: https://tsch.js.org/14188
*/

/* _____________ Your Code Here _____________ */
namespace RLE {
  export type Encode<S extends string> = S extends `${infer Head}${infer Rest}`
    ? EncodeHelper<Rest, Head>
    : S;
  export type Decode<S extends string> = DecodeHelper<S>;
}

type EncodeHelper<
  S extends string,
  Last extends string,
  Count extends readonly unknown[] = [unknown],
  Acc extends string = '',
> = S extends `${infer Head}${infer Rest}`
  ? Last extends Head
    ? EncodeHelper<Rest, Last, [...Count, unknown], Acc>
    : EncodeHelper<Rest, Head, [unknown], `${Acc}${EncodeChar<Last, Count>}`>
  : `${Acc}${EncodeChar<Last, Count>}`;

type DecodeHelper<
  S extends string,
  Acc extends string = '',
> = S extends `${infer N extends number}${infer Rest}`
  ? Rest extends `${infer Head}${infer RRest}`
    ? DecodeHelper<RRest, `${Acc}${Repeat<Head, N>}`>
    : never
  : S extends `${infer Head}${infer Rest}`
  ? DecodeHelper<Rest, `${Acc}${Head}`>
  : Acc;

type EncodeChar<
  C extends string,
  Count extends readonly unknown[],
> = Count['length'] extends 1 ? C : `${Count['length']}${C}`;

type Repeat<
  C extends string,
  N extends number,
  Acc extends readonly unknown[] = [],
> = Acc['length'] extends N ? '' : `${C}${Repeat<C, N, [...Acc, unknown]>}`;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  // Raw string -> encoded string
  Expect<Equal<RLE.Encode<'AAABCCXXXXXXY'>, '3AB2C6XY'>>,

  // Encoded string -> decoded string
  Expect<Equal<RLE.Decode<'3AB2C6XY'>, 'AAABCCXXXXXXY'>>,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/14188/answer
  > View solutions: https://tsch.js.org/14188/solutions
  > More Challenges: https://tsch.js.org
*/
