/*
  35314 - Valid Sudoku
  -------
  by Apollo Wayne (@Shinerising) #hard #array #math #game

  ### Question

  Sudoku is a famous paper and pencil game. The goal of Sudoku is to fill a 9x9 grid with numbers so that each row, column and 3x3 section contain all of the digits between 1 and 9. You'll be given a finished 9x9 grid, and you need to determine if it's a valid Sudoku solution. Return true if it is, and false if it is not.

  ```ts
  type Matrix = [
    [9, 5, 7, 8, 4, 6, 1, 3, 2],
    [2, 3, 4, 5, 9, 1, 6, 7, 8],
    [1, 8, 6, 7, 3, 2, 5, 4, 9],
    [8, 9, 1, 6, 2, 3, 4, 5, 7],
    [3, 4, 5, 9, 7, 8, 2, 6, 1],
    [6, 7, 2, 1, 5, 4, 8, 9, 3],
    [4, 6, 8, 3, 1, 9, 7, 2, 5],
    [5, 2, 3, 4, 8, 7, 9, 1, 6],
    [7, 1, 9, 2, 6, 5, 3, 8, 4]
  ]
  type result = ValidSudoku <Matrix>; // expected to be true
  ```

  > View on GitHub: https://tsch.js.org/35314
*/

/* _____________ Your Code Here _____________ */
type Digit = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9;

type ValidSudoku<T extends Digit[][]> =
  ValidateRows<T> extends false
    ? false
    : ValidateCols<T> extends false
      ? false
      : ValidateRegions<T>;

type ValidateRows<T extends Digit[][]> = MyEqual<
  {
    [R in keyof T]: MyEqual<T[R][number], Digit>;
  }[number],
  true
>;

type ValidateCols<T extends Digit[][]> = ValidateRows<Transpose<T>>;

// prettier-ignore
type Regions = [
  [0, 0], [0, 3], [0, 6],
  [3, 0], [3, 3], [3, 6],
  [6, 0], [6, 3], [6, 6],
]

type ValidateRegions<
  T extends Digit[][],
  R extends Array<[number, number]> = Regions,
> = R extends [
  [infer Row extends number, infer Col extends number],
  ...infer Rest extends Array<[number, number]>,
]
  ? MyEqual<CollectRegion<T, Row, Col>, Digit> extends false
    ? false
    : ValidateRegions<T, Rest>
  : true;

type CollectRegion<
  T extends Digit[][],
  Row extends number,
  Col extends number,
> =
  Next<Row, 3> extends infer Rows extends number
    ? Next<Col, 3> extends infer Cols extends number
      ? {
          [R in Rows]: {
            [C in Cols]: T[R][C];
          }[Cols];
        }[Rows]
      : never
    : never;

type Next<
  T extends number,
  N extends number,
  Value extends readonly unknown[] = NumberToTuple<T>,
  Acc extends number[] = [],
> = Acc['length'] extends N
  ? Acc[number]
  : Next<T, N, [...Value, unknown], [...Acc, Value['length']]>;

type NumberToTuple<
  T extends number,
  Acc extends readonly unknown[] = [],
> = Acc['length'] extends T ? Acc : NumberToTuple<T, [...Acc, unknown]>;

type Transpose<M extends unknown[][]> = (
  M['length'] extends 0 ? [] : M[0]
) extends infer FirstRow extends unknown[]
  ? {
      [X in keyof FirstRow]: {
        [Y in keyof M]: X extends keyof M[Y] ? M[Y][X] : never;
      };
    }
  : never;

type MyEqual<X, Y> =
  (<T>() => T extends X ? 1 : 2) extends <T>() => T extends Y ? 1 : 2
    ? true
    : false;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type Matrix0 = [
  [9, 5, 7, 8, 4, 6, 1, 3, 2],
  [2, 3, 4, 5, 9, 1, 6, 7, 8],
  [1, 8, 6, 7, 3, 2, 5, 4, 9],
  [8, 9, 1, 6, 2, 3, 4, 5, 7],
  [3, 4, 5, 9, 7, 8, 2, 6, 1],
  [6, 7, 2, 1, 5, 4, 8, 9, 3],
  [4, 6, 8, 3, 1, 9, 7, 2, 5],
  [5, 2, 3, 4, 8, 7, 9, 1, 6],
  [7, 1, 9, 2, 6, 5, 3, 8, 4],
];

type Matrix1 = [
  [8, 3, 6, 9, 5, 1, 7, 4, 2],
  [4, 7, 5, 2, 6, 3, 1, 8, 9],
  [2, 1, 9, 7, 4, 8, 6, 3, 5],
  [7, 6, 2, 8, 3, 5, 9, 1, 4],
  [5, 9, 4, 1, 7, 6, 8, 2, 3],
  [3, 8, 1, 4, 2, 9, 5, 7, 6],
  [9, 4, 3, 6, 8, 7, 2, 5, 1],
  [6, 5, 8, 3, 1, 2, 4, 9, 7],
  [1, 2, 7, 5, 9, 4, 3, 6, 8],
];

type Matrix2 = [
  [8, 3, 6, 9, 5, 1, 7, 4, 2],
  [4, 7, 5, 2, 6, 3, 1, 8, 9],
  [2, 1, 9, 7, 4, 8, 6, 3, 5],
  [7, 6, 2, 8, 3, 5, 9, 1, 4],
  [5, 9, 4, 1, 6, 7, 8, 2, 3],
  [3, 8, 1, 4, 2, 9, 5, 7, 6],
  [9, 4, 3, 6, 8, 7, 2, 5, 1],
  [6, 5, 8, 3, 1, 2, 4, 9, 7],
  [1, 2, 7, 5, 9, 4, 3, 6, 8],
];

type Matrix3 = [
  [1, 2, 3, 4, 5, 6, 7, 8, 9],
  [2, 3, 4, 5, 6, 7, 8, 9, 1],
  [3, 4, 5, 6, 7, 8, 9, 1, 2],
  [4, 5, 6, 7, 8, 9, 1, 2, 3],
  [5, 6, 7, 8, 9, 1, 2, 3, 4],
  [6, 7, 8, 9, 1, 2, 3, 4, 5],
  [7, 8, 9, 1, 2, 3, 4, 5, 6],
  [8, 9, 1, 2, 3, 4, 5, 6, 7],
  [9, 1, 2, 3, 4, 5, 6, 7, 8],
];

type cases = [
  Expect<Equal<ValidSudoku<Matrix0>, true>>,
  Expect<Equal<ValidSudoku<Matrix1>, true>>,
  Expect<Equal<ValidSudoku<Matrix2>, false>>,
  Expect<Equal<ValidSudoku<Matrix3>, false>>,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/35314/answer
  > View solutions: https://tsch.js.org/35314/solutions
  > More Challenges: https://tsch.js.org
*/
