/*
  14080 - FizzBuzz
  -------
  by Lee Crosby (@Cygnut) #hard #array #math #infer

  ### Question

  The FizzBuzz problem is a classic test given in coding interviews. The task is simple:

  Print integers 1 to N, except:

  - Print "Fizz" if an integer is divisible by 3;
  - Print "Buzz" if an integer is divisible by 5;
  - Print "FizzBuzz" if an integer is divisible by both 3 and 5.

  For example, for N = 20, the output should be:
  `1, 2, Fizz, 4, Buzz, Fizz, 7, 8, Fizz, Buzz, 11, Fizz, 13, 14, FizzBuzz, 16, 17, Fizz, 19, Buzz`

  In the challenge below, we will want to generate this as an array of string literals.

  For large values of N, you will need to ensure that any types generated do so efficiently (e.g. by
  correctly using the tail-call optimisation for recursion).

  > View on GitHub: https://tsch.js.org/14080
*/

/* _____________ Your Code Here _____________ */
type FizzBuzz<N extends number> = NumberToTuple<N> extends infer T extends
  readonly unknown[]
  ? Merge<[Fizz<T>, Buzz<T>]> extends infer Merged extends readonly unknown[]
    ? Take<Drop<Merged, 1>, N>
    : never
  : never;

type Fizz<
  N extends readonly unknown[],
  Acc extends readonly string[] = [never],
> = N['length'] extends 0
  ? Acc
  : Fizz<Drop<N, 3>, [...Acc, never, never, 'Fizz']>;

type Buzz<
  N extends readonly unknown[],
  Acc extends readonly string[] = [never],
> = N['length'] extends 0
  ? Acc
  : Buzz<Drop<N, 5>, [...Acc, never, never, never, never, 'Buzz']>;

type Merge<Parts extends ReadonlyArray<readonly string[]>> =
  Transpose<Parts> extends infer Rows extends ReadonlyArray<readonly string[]>
    ? {
        [Key in keyof Rows]: MergeRow<Rows[Key]> extends infer Result
          ? Result extends ''
            ? Key
            : Result
          : never;
      }
    : never;

type MergeRow<
  Row extends readonly string[],
  Acc extends string = ``,
> = Row extends [
  infer Head extends string,
  ...infer Rest extends readonly string[],
]
  ? MergeRow<Rest, [Head] extends [never] ? Acc : `${Acc}${Head}`>
  : Acc;

type Transpose<T extends ReadonlyArray<readonly unknown[]>> = (
  T['length'] extends 0 ? [] : T[0]
) extends infer FirstRow extends readonly unknown[]
  ? {
      [X in keyof FirstRow]: {
        [Y in keyof T]: X extends keyof T[Y] ? T[Y][X] : never;
      };
    }
  : never;

type NumberToTuple<
  N extends number,
  Acc extends readonly unknown[] = [],
> = Acc['length'] extends N ? Acc : NumberToTuple<N, [...Acc, unknown]>;

type Take<
  T extends readonly unknown[],
  N extends number,
> = T['length'] extends N
  ? T
  : T extends [...infer Init, unknown]
  ? Take<Init, N>
  : T;

type Drop<
  T extends readonly unknown[],
  N extends number,
  Acc extends readonly unknown[] = [],
> = Acc['length'] extends N
  ? T
  : T extends [unknown, ...infer Rest]
  ? Drop<Rest, N, [...Acc, unknown]>
  : T;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<FizzBuzz<1>, ['1']>>,
  Expect<Equal<FizzBuzz<5>, ['1', '2', 'Fizz', '4', 'Buzz']>>,
  Expect<
    Equal<
      FizzBuzz<20>,
      [
        '1',
        '2',
        'Fizz',
        '4',
        'Buzz',
        'Fizz',
        '7',
        '8',
        'Fizz',
        'Buzz',
        '11',
        'Fizz',
        '13',
        '14',
        'FizzBuzz',
        '16',
        '17',
        'Fizz',
        '19',
        'Buzz',
      ]
    >
  >,
  Expect<
    Equal<
      FizzBuzz<100>,
      [
        '1',
        '2',
        'Fizz',
        '4',
        'Buzz',
        'Fizz',
        '7',
        '8',
        'Fizz',
        'Buzz',
        '11',
        'Fizz',
        '13',
        '14',
        'FizzBuzz',
        '16',
        '17',
        'Fizz',
        '19',
        'Buzz',
        'Fizz',
        '22',
        '23',
        'Fizz',
        'Buzz',
        '26',
        'Fizz',
        '28',
        '29',
        'FizzBuzz',
        '31',
        '32',
        'Fizz',
        '34',
        'Buzz',
        'Fizz',
        '37',
        '38',
        'Fizz',
        'Buzz',
        '41',
        'Fizz',
        '43',
        '44',
        'FizzBuzz',
        '46',
        '47',
        'Fizz',
        '49',
        'Buzz',
        'Fizz',
        '52',
        '53',
        'Fizz',
        'Buzz',
        '56',
        'Fizz',
        '58',
        '59',
        'FizzBuzz',
        '61',
        '62',
        'Fizz',
        '64',
        'Buzz',
        'Fizz',
        '67',
        '68',
        'Fizz',
        'Buzz',
        '71',
        'Fizz',
        '73',
        '74',
        'FizzBuzz',
        '76',
        '77',
        'Fizz',
        '79',
        'Buzz',
        'Fizz',
        '82',
        '83',
        'Fizz',
        'Buzz',
        '86',
        'Fizz',
        '88',
        '89',
        'FizzBuzz',
        '91',
        '92',
        'Fizz',
        '94',
        'Buzz',
        'Fizz',
        '97',
        '98',
        'Fizz',
        'Buzz',
      ]
    >
  >,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/14080/answer
  > View solutions: https://tsch.js.org/14080/solutions
  > More Challenges: https://tsch.js.org
*/
