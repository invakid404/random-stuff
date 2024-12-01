type $Equal<X, Y> =
  (<T>() => T extends X ? 1 : 2) extends <T>() => T extends Y ? 1 : 2
    ? true
    : false;

type Fn = (arg: never) => unknown;

declare const _: unique symbol;

type _ = typeof _;

declare abstract class HKT<F extends Fn = Fn> {
  abstract readonly [_]: unknown;
  fn: F;
}

type InputOf<T extends HKT> = Parameters<T["fn"]>[0];

export type $<T extends HKT, U extends InputOf<T>> = ReturnType<
  (T & { [_]: U })["fn"]
>;

type Cast<T, U> = T extends U ? T : U;

export interface SplitBy extends HKT {
  fn: (delimiter: Cast<this[_], string>) => SplitImpl<typeof delimiter>;
}

interface SplitImpl<D extends string> extends HKT {
  fn: (input: Cast<this[_], string>) => $Split<typeof input, D>;
}

type $Split<
  T extends string,
  D extends string,
  Acc extends readonly string[] = [],
> = T extends `${infer Left}${D}${infer Right}`
  ? $Split<Right, D, "" extends Left ? Acc : [...Acc, Left]>
  : "" extends T
    ? Acc
    : [...Acc, T];

export interface MapWith extends HKT {
  fn: (op: Cast<this[_], HKT>) => MapImpl<typeof op>;
}

interface MapImpl<Op extends HKT> extends HKT {
  fn: (input: Cast<this[_], Array<InputOf<Op>>>) => $Map<typeof input, Op>;
}

type $Map<
  Input extends Array<InputOf<Op>>,
  Op extends HKT,
  Acc extends readonly unknown[] = [],
> = Input extends [
  infer Head extends InputOf<Op>,
  ...infer Rest extends Array<InputOf<Op>>,
]
  ? $Map<Rest, Op, [...Acc, $<Op, Head>]>
  : Acc;

export interface Chain extends HKT {
  fn: (ops: Cast<this[_], HKT[]>) => ChainImpl<typeof ops>;
}

interface ChainImpl<Ops extends HKT[]> extends HKT {
  fn: (input: Cast<this[_], InputOf<Ops[0]>>) => $Chain<typeof input, Ops>;
}

type $Chain<T extends InputOf<Ops[0]>, Ops extends HKT[]> = Ops extends [
  infer Head extends HKT,
  ...infer Rest extends HKT[],
]
  ? $<Head, T> extends infer R
    ? [] extends Rest
      ? R
      : R extends InputOf<Rest[0]>
        ? $Chain<R, Rest>
        : never
    : never
  : T;

export interface Transpose extends HKT {
  fn: (input: Cast<this[_], unknown[][]>) => $Transpose<typeof input>;
}

type $Transpose<T extends unknown[][]> = (
  T["length"] extends 0 ? [] : T[0]
) extends infer First extends unknown[]
  ? {
      [X in keyof First]: {
        [Y in keyof T]: X extends keyof T[Y] ? T[Y][X] : never;
      };
    }
  : never;

export interface ToNumber extends HKT {
  fn: (input: Cast<this[_], string>) => $ToNumber<typeof input>;
}

type $ToNumber<T extends string> = T extends ""
  ? never
  : $TrimLeft<T, "0"> extends infer T
    ? "" extends T
      ? 0
      : T extends `${infer N extends number}`
        ? N
        : never
    : never;

export interface Digits extends HKT {
  fn: (input: Cast<this[_], number>) => $Digits<`${typeof input}`>;
}

type Digit = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9;

type StringToDigit = { [Key in Digit as `${Key}`]: Key };

type $Digits<
  Value extends string,
  Acc extends number[] = [],
> = Value extends `${infer Head extends keyof StringToDigit}${infer Rest}`
  ? $Digits<Rest, [...Acc, StringToDigit[Head]]>
  : Acc;

export interface Not extends HKT {
  fn: (input: Cast<this[_], boolean>) => $Not<typeof input>;
}

type $Not<T extends boolean> = true extends T ? false : true;

export interface Compare extends HKT {
  fn: (
    input: Cast<this[_], [number, number]>,
  ) => $Compare<(typeof input)[0], (typeof input)[1]>;
}

type $Compare<
  L extends string | number | bigint,
  R extends string | number | bigint,
> = `${L}` extends `${R}`
  ? CompareResult.Eq
  : SmallerThanOrEqual<L, R> extends true
    ? CompareResult.Lt
    : CompareResult.Gt;

type SmallerThanOrEqual<
  Left extends string | number | bigint,
  Right extends string | number | bigint,
> =
  AbsWithSign<Left> extends [
    infer AbsLeft extends string | number | bigint,
    infer SignLeft extends boolean,
  ]
    ? AbsWithSign<Right> extends [
        infer AbsRight extends string | number | bigint,
        infer SignRight extends boolean,
      ]
      ? SignLeft extends SignRight
        ? CompareDigitSlices<
            $Digits<`${AbsLeft}`>,
            $Digits<`${AbsRight}`>
          > extends infer Result extends boolean
          ? SignLeft extends true
            ? Result
            : $Not<Result>
          : never
        : SignRight
      : never
    : never;

type CompareDigitSlices<Left extends Digit[], Right extends Digit[]> =
  PadDigitSlices<Left, Right> extends [
    infer PaddedLeft extends Digit[],
    infer PaddedRight extends Digit[],
  ]
    ? CompareDigitSlicesHelper<PaddedLeft, PaddedRight>
    : never;

type CompareDigitSlicesHelper<Left extends Digit[], Right extends Digit[]> =
  BothEmpty<Left, Right> extends true
    ? true
    : Left extends [
          infer LeftHead extends Digit,
          ...infer LeftRest extends Digit[],
        ]
      ? Right extends [
          infer RightHead extends Digit,
          ...infer RightRest extends Digit[],
        ]
        ? LeftHead extends RightHead
          ? CompareDigitSlicesHelper<LeftRest, RightRest>
          : ComparisonTable[LeftHead][RightHead]
        : never
      : never;

type ComparisonTable = {
  [D in Digit]: ComparisonRow<D>;
};

type ComparisonRow<
  Value extends Digit,
  Acc extends readonly boolean[] = [],
  Bigger extends boolean = false,
> = Acc["length"] extends 10
  ? Acc
  : (
        Value extends Acc["length"] ? true : Bigger
      ) extends infer NewBigger extends boolean
    ? ComparisonRow<Value, [...Acc, NewBigger], NewBigger>
    : never;

type PadDigitSlices<
  Left extends Digit[],
  Right extends Digit[],
  LeftAcc extends Digit[] = [],
  RightAcc extends Digit[] = [],
> =
  BothEmpty<Left, Right> extends true
    ? [LeftAcc, RightAcc]
    : PadDigitSlices<
        Init<Left>,
        Init<Right>,
        [LastOrDefault<Left, 0>, ...LeftAcc],
        [LastOrDefault<Right, 0>, ...RightAcc]
      >;

type BothEmpty<Left extends any[], Right extends any[]> = Left extends []
  ? Right extends []
    ? true
    : false
  : false;

type Init<T extends unknown[]> = T extends [...infer I, unknown] ? I : [];

type LastOrDefault<T extends unknown[], U> = T extends [...unknown[], infer L]
  ? L
  : U;

export interface Abs extends HKT {
  fn: (
    input: Cast<this[_], number>,
  ) => AbsWithSign<typeof input> extends [infer R, boolean] ? R : never;
}

type AbsWithSign<T extends number | bigint | string> =
  `${T}` extends `-${infer Abs extends number | bigint}`
    ? [Abs, false]
    : [T, true];

export declare enum CompareResult {
  Eq,
  Lt,
  Gt,
}

export interface Sort extends HKT {
  fn: (
    input: Cast<this[_], number[] | [number[], boolean]>,
  ) => typeof input extends [infer V extends number[], infer D extends boolean]
    ? $Sort<V, D>
    : typeof input extends number[]
      ? $Sort<typeof input>
      : never;
}

type $Sort<
  Array extends number[],
  Direction extends boolean = false,
> = [] extends Array
  ? []
  : Partition<Array, 0, Direction> extends [
        infer Left extends number[],
        infer Right extends number[],
      ]
    ? [...$Sort<Left, Direction>, Array[0], ...$Sort<Right, Direction>]
    : never;

type Partition<
  Array extends number[],
  PivotIndex extends number,
  Direction extends boolean,
> = PartitionWithValue<
  DropAt<[...Array], PivotIndex>,
  Array[PivotIndex],
  Direction
>;

type PartitionWithValue<
  Array extends number[],
  Pivot extends number,
  Direction extends boolean,
  Left extends readonly any[] = [],
  Right extends readonly any[] = [],
> = Array extends [infer Head extends number, ...infer Tail extends number[]]
  ? CompareWithDirection<Head, Pivot, Direction> extends CompareResult.Lt
    ? PartitionWithValue<Tail, Pivot, Direction, [...Left, Head], Right>
    : PartitionWithValue<Tail, Pivot, Direction, Left, [...Right, Head]>
  : [Left, Right];

type DropAt<Array extends unknown[], Index extends number> =
  SplitAt<Array, Index> extends [
    infer Left extends unknown[],
    infer Right extends unknown[],
  ]
    ? [...Left, ...Tail<Right>]
    : never;

type SplitAt<
  Arr extends unknown[],
  Index extends number,
  InitAccumulator extends any[] = [],
> = Arr extends []
  ? [InitAccumulator, Arr]
  : InitAccumulator["length"] extends Index
    ? [InitAccumulator, Arr]
    : SplitAt<Tail<Arr>, Index, [...InitAccumulator, Arr[0]]>;

type Tail<T extends unknown[]> = T extends [unknown, ...infer T] ? T : never;

type CompareWithDirection<
  A extends number,
  B extends number,
  Direction extends boolean,
> =
  $Compare<A, B> extends infer Result
    ? Result extends CompareResult.Eq
      ? Result
      : Direction extends false
        ? Result
        : Result extends CompareResult.Lt
          ? CompareResult.Gt
          : CompareResult.Lt
    : never;

export interface Add extends HKT {
  fn: (
    input: Cast<this[_], [number, number]>,
  ) => $Add<(typeof input)[0], (typeof input)[1]>;
}

type $Add<
  Left extends number | bigint | string,
  Right extends number | bigint | string,
> =
  AbsWithSign<Left> extends [
    infer LeftAbs extends string | bigint | number,
    infer LeftSign extends boolean,
  ]
    ? AbsWithSign<Right> extends [
        infer RightAbs extends string | bigint | number,
        infer RightSign extends boolean,
      ]
      ? $Equal<LeftSign, RightSign> extends true
        ? ApplySign<AddInner<LeftAbs, RightAbs>, LeftSign>
        : LeftSign extends true
          ? $Subtract<LeftAbs, RightAbs>
          : $Subtract<RightAbs, LeftAbs>
      : never
    : never;

type AddInner<
  Left extends number | bigint | string,
  Right extends number | bigint | string,
> =
  SumDigitSlices<
    $Digits<`${Left}`>,
    $Digits<`${Right}`>
  > extends infer Result extends Digit[]
    ? $ToNumber<DigitsToString<Result>>
    : never;

type ApplySign<
  N extends number | bigint | string,
  Sign extends boolean,
> = true extends Sign ? N : $ToNumber<`-${N}`>;

type SumDigitSlices<
  Left extends Digit[],
  Right extends Digit[],
  Carry extends boolean = false,
> =
  BothEmpty<Left, Right> extends true
    ? Carry extends true
      ? [1]
      : []
    : SumDigits<
          LastOrDefault<Left, 0>,
          LastOrDefault<Right, 0>,
          Carry
        > extends [infer Result, infer Carry extends boolean]
      ? [...SumDigitSlices<Init<Left>, Init<Right>, Carry>, Result]
      : never;

type SumDigits<
  Left extends Digit,
  Right extends Digit,
  Carry extends boolean,
> = Carry extends true
  ? SumDigitsWithCarry<Left, Right>
  : SumDigitsWithoutCarry<Left, Right>;

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

type SumDigitsWithCarry<Left extends Digit, Right extends Digit> =
  SumDigitsWithoutCarry<Left, Right> extends [
    infer Result extends Digit,
    infer Carry extends boolean,
  ]
    ? Successor[Result] extends infer ResultSuccessor extends Digit
      ? [
          ResultSuccessor,
          Carry extends true ? Carry : ResultSuccessor extends 0 ? true : false,
        ]
      : never
    : never;

type Predecessor = {
  0: 9;
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
  9: 0;
};

type DigitsToString<
  Digits extends Digit[],
  Acc extends string = "",
> = Digits extends [infer Head extends Digit, ...infer Tail extends Digit[]]
  ? DigitsToString<Tail, `${Acc}${Head}`>
  : $TrimLeft<Acc, "0"> extends infer R
    ? "" extends R
      ? "0"
      : R
    : never;

export interface Subtract extends HKT {
  fn: (
    input: Cast<this[_], [number, number]>,
  ) => $Subtract<(typeof input)[0], (typeof input)[1]>;
}

type $Subtract<
  Left extends number | bigint | string,
  Right extends number | bigint | string,
> =
  AbsWithSign<Left> extends [
    infer LeftAbs extends string | bigint | number,
    infer LeftSign extends boolean,
  ]
    ? AbsWithSign<Right> extends [
        infer RightAbs extends string | bigint | number,
        infer RightSign extends boolean,
      ]
      ? $Equal<LeftSign, RightSign> extends true
        ? $Equal<LeftSign, true> extends true
          ? $Compare<LeftAbs, RightAbs> extends CompareResult.Lt
            ? ApplySign<SubtractInner<RightAbs, LeftAbs>, false>
            : SubtractInner<LeftAbs, RightAbs>
          : $Subtract<RightAbs, LeftAbs>
        : ApplySign<$Add<LeftAbs, RightAbs>, LeftSign>
      : never
    : never;

type SubtractInner<
  Left extends number | bigint | string,
  Right extends number | bigint | string,
> =
  SubtractDigitSlices<
    $Digits<`${Left}`>,
    $Digits<`${Right}`>
  > extends infer Result extends Digit[]
    ? $ToNumber<DigitsToString<Result>>
    : never;

type SubtractDigitSlices<
  Left extends Digit[],
  Right extends Digit[],
  Borrow extends boolean = false,
> =
  BothEmpty<Left, Right> extends true
    ? []
    : SubtractDigits<
          LastOrDefault<Left, 0>,
          LastOrDefault<Right, 0>,
          Borrow
        > extends [infer Result, infer NextBorrow extends boolean]
      ? [...SubtractDigitSlices<Init<Left>, Init<Right>, NextBorrow>, Result]
      : never;

type SubtractDigits<
  Left extends Digit,
  Right extends Digit,
  Borrow extends boolean,
> = Borrow extends true
  ? SubtractDigitsWithBorrow<Left, Right>
  : SubtractDigitsWithoutBorrow<Left, Right>;

type SubtractDigitsWithoutBorrow<
  Left extends Digit,
  Right extends Digit,
  Borrow extends boolean = false,
> = Right extends 0
  ? [Left, Borrow]
  : Predecessor[Left] extends infer LeftPred extends Digit
    ? SubtractDigitsWithoutBorrow<
        LeftPred,
        Predecessor[Right],
        LeftPred extends 9 ? true : Borrow
      >
    : never;

type SubtractDigitsWithBorrow<
  Left extends Digit,
  Right extends Digit,
> = Predecessor[Left] extends infer LeftPred extends Digit
  ? SubtractDigitsWithoutBorrow<
      LeftPred,
      Right,
      LeftPred extends 9 ? true : false
    >
  : never;

export interface TrimLeft extends HKT {
  fn: (char: Cast<this[_], string>) => TrimLeftImpl<typeof char>;
}

interface TrimLeftImpl<C extends string> extends HKT {
  fn: (input: Cast<this[_], string>) => $TrimLeft<typeof input, C>;
}

type $TrimLeft<
  T extends string,
  C extends string,
> = T extends `${C}${infer Rest}` ? $TrimLeft<Rest, C> : T;

export interface Reverse extends HKT {
  fn: (input: Cast<this[_], unknown[]>) => $Reverse<typeof input>;
}

type $Reverse<T extends unknown[], Acc extends unknown[] = []> = T extends [
  infer Head,
  ...infer Rest,
]
  ? $Reverse<Rest, [Head, ...Acc]>
  : Acc;

export interface Reduce extends HKT {
  fn: (
    input: Cast<this[_], [HKT<(arg: [any, any]) => any>, any]>,
  ) => ReduceImpl<(typeof input)[0], (typeof input)[1]>;
}

interface ReduceImpl<
  Op extends HKT<(arg: [any, any]) => any>,
  Init extends InputOf<Op>[number],
> extends HKT {
  fn: (
    input: Cast<this[_], Array<InputOf<Op>[number]>>,
  ) => $Reduce<typeof input, Op, Init>;
}

type $Reduce<
  T extends Array<InputOf<Op>[number]>,
  Op extends HKT<(arg: [any, any]) => any>,
  Acc extends InputOf<Op>[number],
> = T extends [
  infer Head extends InputOf<Op>[number],
  ...infer Rest extends Array<InputOf<Op>[number]>,
]
  ? $Reduce<Rest, Op, $<Op, [Acc, Head]>>
  : Acc;

type HKTWithImpl = HKT<(arg: never) => HKT>;

export interface Flip extends HKT {
  fn: (input: Cast<this[_], HKTWithImpl>) => Flipped<typeof input>;
}

interface Flipped<Op extends HKTWithImpl> extends HKT {
  fn: (
    innerInput: Cast<this[_], InputOf<ReturnType<(Op & { [_]: any })["fn"]>>>,
  ) => FlippedImpl<Op, typeof innerInput>;
}

interface FlippedImpl<Op extends HKTWithImpl, InnerInput> extends HKT {
  fn: (
    outerInput: Cast<this[_], InputOf<Op>>,
  ) => $<$<Op, typeof outerInput>, InnerInput>;
}

export interface Equal extends HKT {
  fn: (input: Cast<this[_], unknown>) => EqualImpl<typeof input>;
}

export interface EqualImpl<T> extends HKT {
  fn: (input: Cast<this[_], unknown>) => $Equal<T, typeof input>;
}

export interface Filter extends HKT {
  fn: (op: Cast<this[_], HKT>) => FilterImpl<typeof op>;
}

interface FilterImpl<Op extends HKT> extends HKT {
  fn: (input: Cast<this[_], Array<InputOf<Op>>>) => $Filter<typeof input, Op>;
}

type $Filter<
  T extends Array<InputOf<Op>>,
  Op extends HKT,
  Acc extends unknown[] = [],
> = T extends [
  infer Head extends InputOf<Op>,
  ...infer Rest extends Array<InputOf<Op>>,
]
  ? $Filter<
      Rest,
      Op,
      $Equal<$<Op, Head>, false> extends true ? Acc : [...Acc, Head]
    >
  : Acc;
