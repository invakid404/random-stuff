import { Parse, Parser, ParserInput, ParserResult } from "./parser.js";

type $Equal<X, Y> =
  (<T>() => T extends X ? 1 : 2) extends <T>() => T extends Y ? 1 : 2
    ? true
    : false;

type Fn = (arg: never) => unknown;

export declare const _: unique symbol;

export type _ = typeof _;

export declare abstract class HKT<F extends Fn = Fn> {
  abstract readonly [_]: unknown;
  fn: F;
}

type InputOf<T extends HKT> = Parameters<T["fn"]>[0];

export type $<T extends HKT, U extends InputOf<T>> = ReturnType<
  (T & { [_]: U })["fn"]
>;

export type Cast<T, U> = T extends U ? T : U;

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

export interface MapWith_ extends HKT {
  fn: (
    input: Cast<this[_], [HKT, any[]]>,
  ) => $Map<(typeof input)[1], (typeof input)[0]>;
}

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
  $DropAt<[...Array], PivotIndex>,
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

type $DropAt<Array extends unknown[], Index extends number> =
  SplitAt<Array, Index> extends [
    infer Left extends unknown[],
    infer Right extends unknown[],
  ]
    ? [...Left, ...$Tail<Right>]
    : never;

type SplitAt<
  Arr extends unknown[],
  Index extends number,
  InitAccumulator extends any[] = [],
> = Arr extends []
  ? [InitAccumulator, Arr]
  : InitAccumulator["length"] extends Index
    ? [InitAccumulator, Arr]
    : SplitAt<$Tail<Arr>, Index, [...InitAccumulator, Arr[0]]>;

type $Tail<T extends unknown[]> = T extends [unknown, ...infer T] ? T : never;

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

export interface AddTo extends HKT {
  fn: (input: Cast<this[_], number>) => AddToImpl<typeof input>;
}

interface AddToImpl<L extends number> extends HKT {
  fn: (input: Cast<this[_], number>) => $Add<L, typeof input>;
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

export interface MapReduce extends HKT {
  fn: (
    input: Cast<
      this[_],
      [HKT<(arg: any) => any>, HKT<(arg: [any, any]) => any>, any]
    >,
  ) => MapReduceImpl<(typeof input)[0], (typeof input)[1], (typeof input)[2]>;
}

interface MapReduceImpl<
  MapOp extends HKT<(any: any) => any>,
  ReduceOp extends HKT<(arg: [any, any]) => any>,
  Init extends InputOf<MapOp>,
> extends HKT {
  fn: (
    input: Cast<this[_], Array<InputOf<MapOp>>>,
  ) => $MapReduce<typeof input, MapOp, ReduceOp, Init>;
}

type $MapReduce<
  T extends readonly any[],
  MapOp extends HKT<(any: any) => any>,
  ReduceOp extends HKT<(arg: [any, any]) => any>,
  Acc extends any,
> = T extends [infer Head, ...infer Rest]
  ? $MapReduce<Rest, MapOp, ReduceOp, $<ReduceOp, [Acc, $<MapOp, Head>]>>
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

export interface Equal_ extends HKT {
  fn: (
    input: Cast<this[_], [unknown, unknown]>,
  ) => $Equal<(typeof input)[0], (typeof input)[1]>;
}

export interface DiffObjects_ extends HKT {
  fn: (
    input: Cast<this[_], [object, object]>,
  ) => $DiffObjects<(typeof input)[0], (typeof input)[1]>;
}

type $DiffObjects<T, U> = {
  [P in Exclude<keyof T, keyof U>]: T[P];
} & {
  [P in Exclude<keyof U, keyof T>]: U[P];
} & {
  [P in Extract<keyof T, keyof U> as $<Equal_, [T[P], U[P]]> extends false
    ? P
    : never]:
    | Exclude<T[P], Extract<T[P], U[P]>>
    | Exclude<U[P], Extract<U[P], T[P]>>;
};

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

export interface Windows extends HKT {
  fn: (length: Cast<this[_], number>) => WindowsImpl<typeof length>;
}

interface WindowsImpl<L extends number> extends HKT {
  fn: (input: Cast<this[_], unknown[]>) => $Windows<typeof input, L>;
}

type $Windows<
  T extends unknown[],
  L extends number,
  Acc extends unknown[][] = [],
> =
  Take<T, L> extends infer W extends unknown[]
    ? L extends W["length"]
      ? $Windows<$Tail<T>, L, [...Acc, W]>
      : Acc
    : never;

type Take<
  T extends unknown[],
  L extends number,
  Acc extends unknown[] = [],
> = L extends Acc["length"]
  ? Acc
  : T extends [infer Head, ...infer Rest]
    ? Take<Rest, L, [...Acc, Head]>
    : Acc;

export interface LessThan extends HKT {
  fn: (value: Cast<this[_], number>) => LessThanImpl<typeof value>;
}

interface LessThanImpl<V extends number> extends HKT {
  fn: (
    value: Cast<this[_], number>,
  ) => $Compare<typeof value, V> extends CompareResult.Lt ? true : false;
}

export interface GreaterThan extends HKT {
  fn: (value: Cast<this[_], number>) => GreaterThanImpl<typeof value>;
}

interface GreaterThanImpl<V extends number> extends HKT {
  fn: (
    value: Cast<this[_], number>,
  ) => $Compare<typeof value, V> extends CompareResult.Gt ? true : false;
}

export interface GreaterThanOrEquals extends HKT {
  fn: (value: Cast<this[_], number>) => GreaterThanOrEqualsImpl<typeof value>;
}

interface GreaterThanOrEqualsImpl<V extends number> extends HKT {
  fn: (
    value: Cast<this[_], number>,
  ) => $Compare<typeof value, V> extends CompareResult.Lt ? false : true;
}

export interface And extends HKT {
  fn: (ops: Cast<this[_], HKT[]>) => AndImpl<typeof ops>;
}

interface AndImpl<Ops extends HKT[]> extends HKT {
  fn: (input: Cast<this[_], InputOf<Ops[number]>>) => $And<typeof input, Ops>;
}

type $And<T extends InputOf<Ops[number]>, Ops extends HKT[]> = Ops extends [
  infer Head extends HKT,
  ...infer Rest extends HKT[],
]
  ? $Equal<$<Head, T>, false> extends true
    ? false
    : $And<T, Rest>
  : true;

export interface All extends HKT {
  fn: (ops: Cast<this[_], HKT>) => AllImpl<typeof ops>;
}

interface AllImpl<Op extends HKT> extends HKT {
  fn: (input: Cast<this[_], Array<InputOf<Op>>>) => $All<typeof input, Op>;
}

type $All<T extends Array<InputOf<Op>>, Op extends HKT> = T extends [
  infer Head extends InputOf<Op>,
  ...infer Rest extends Array<InputOf<Op>>,
]
  ? $Equal<$<Op, Head>, false> extends true
    ? false
    : $All<Rest, Op>
  : true;

export interface Any extends HKT {
  fn: (ops: Cast<this[_], HKT>) => AnyImpl<typeof ops>;
}

interface AnyImpl<Op extends HKT> extends HKT {
  fn: (input: Cast<this[_], Array<InputOf<Op>>>) => $Any<typeof input, Op>;
}

type $Any<T extends Array<InputOf<Op>>, Op extends HKT> = T extends [
  infer Head extends InputOf<Op>,
  ...infer Rest extends Array<InputOf<Op>>,
]
  ? $Equal<$<Op, Head>, true> extends true
    ? true
    : $Any<Rest, Op>
  : false;

export interface ApplyMany extends HKT {
  fn: (ops: Cast<this[_], HKT[]>) => ApplyManyImpl<typeof ops>;
}

interface ApplyManyImpl<Ops extends HKT[]> extends HKT {
  fn: (
    input: Cast<this[_], InputOf<Ops[number]>>,
  ) => $ApplyMany<typeof input, Ops>;
}

type $ApplyMany<
  T extends InputOf<Ops[number]>,
  Ops extends HKT[],
  Acc extends unknown[] = [],
> = {
  [Key in keyof Ops]: $<Ops[Key], T>;
};

export interface Identity extends HKT {
  fn: (input: Cast<this[_], unknown>) => typeof input;
}

export interface Length extends HKT {
  fn: (input: Cast<this[_], unknown[]>) => $Length<typeof input>;
}

type $Length<T extends unknown[], Acc extends number = 0> = T extends [
  infer Head,
  ...infer Rest,
]
  ? $Length<Rest, $<Add, [Acc, 1]>>
  : Acc;

export interface Range extends HKT {
  fn: (
    input: Cast<this[_], [number, number]>,
  ) => $Range<(typeof input)[0], (typeof input)[1]>;
}

type $Range<L extends number, R extends number> =
  $Compare<L, R> extends CompareResult.Lt ? RangeHelper<L, R> : [];

type RangeHelper<
  L extends number,
  R extends number,
  Acc extends number[] = [],
> = $Equal<L, R> extends true ? Acc : RangeHelper<$Add<L, 1>, R, [...Acc, L]>;

export interface RangeFrom extends HKT {
  fn: (input: Cast<this[_], number>) => RangeFromImpl<typeof input>;
}

interface RangeFromImpl<L extends number> extends HKT {
  fn: (input: Cast<this[_], number>) => $Range<L, typeof input>;
}

export interface At extends HKT {
  fn: (index: Cast<this[_], number>) => AtImpl<typeof index>;
}

interface AtImpl<I extends number> extends HKT {
  fn: (
    input: Cast<this[_], unknown[]>,
  ) => I extends keyof typeof input ? (typeof input)[I] : never;
}

export interface At_ extends HKT {
  fn: (
    input: Cast<this[_], [unknown[], number]>,
  ) => (typeof input)[0][(typeof input)[1]];
}

export interface AtDeep extends HKT {
  fn: (indices: Cast<this[_], number[]>) => AtDeepImpl<typeof indices>;
}

interface AtDeepImpl<I extends number[]> extends HKT {
  fn: (input: Cast<this[_], unknown[]>) => $AtDeep<typeof input, I>;
}

type $AtDeep<T extends unknown[], I extends number[]> = I extends [
  infer Head extends number,
  ...infer Rest extends number[],
]
  ? `${Head}` extends keyof T
    ? 0 extends Rest["length"]
      ? T[Head]
      : T[Head] extends unknown[]
        ? $AtDeep<T[Head], Rest>
        : undefined
    : undefined
  : undefined;

export interface Middle extends HKT {
  fn: (input: Cast<this[_], unknown[]>) => $Middle<typeof input>;
}

type $Middle<T extends unknown[]> = T[MiddleHelper<
  T,
  0,
  $<Subtract, [T["length"], 1]>
>];

type MiddleHelper<T extends unknown[], L extends number, R extends number> =
  $<Compare, [L, R]> extends CompareResult.Lt
    ? MiddleHelper<T, $<Add, [L, 1]>, $<Subtract, [R, 1]>>
    : L;

export interface ToArray extends HKT {
  fn: (input: Cast<this[_], unknown>) => [typeof input];
}

export interface CartesianProduct extends HKT {
  fn: (
    input: Cast<this[_], [unknown[], unknown[]]>,
  ) => $CartesianProduct<(typeof input)[0], (typeof input)[1]>;
}

type $CartesianProduct<
  T extends unknown[],
  U extends unknown[],
> = $FlattenDepth<
  {
    [TK in keyof T]: {
      [UK in keyof U]: [T[TK], U[UK]];
    };
  },
  1
>;

export interface Tail extends HKT {
  fn: (input: Cast<this[_], unknown[]>) => $Tail<typeof input>;
}

export interface Tails extends HKT {
  fn: (input: Cast<this[_], unknown[]>) => $Tails<typeof input>;
}

type $Tails<T extends unknown[], Acc extends unknown[][] = []> = T extends [
  unknown,
  ...infer Rest extends unknown[],
]
  ? $Tails<Rest, [...Acc, Rest]>
  : Acc;

export interface Zip extends HKT {
  fn: (
    input: Cast<this[_], [unknown[], unknown[]]>,
  ) => $Zip<(typeof input)[0], (typeof input)[1]>;
}

type $Zip<L extends unknown[], R extends unknown[]> = {
  [Key in keyof L]: [L[Key], Key extends keyof R ? R[Key] : never];
};

export interface DropAt extends HKT {
  fn: (input: Cast<this[_], number>) => DropAtImpl<typeof input>;
}

interface DropAtImpl<I extends number> extends HKT {
  fn: (input: Cast<this[_], unknown[]>) => $DropAt<typeof input, I>;
}

export interface DropAt_ extends HKT {
  fn: (
    input: Cast<this[_], [unknown[], number]>,
  ) => $DropAt<(typeof input)[0], (typeof input)[1]>;
}

export interface Apply_ extends HKT {
  fn: (
    input: Cast<this[_], [HKT, unknown]>,
  ) => $Apply_<(typeof input)[0], (typeof input)[1]>;
}

type $Apply_<Op extends HKT, Input> =
  Input extends InputOf<Op> ? $<Op, Input> : never;

export interface MustRunParser extends HKT {
  fn: (input: Cast<this[_], Parser>) => MustRunParserImpl<typeof input>;
}

interface MustRunParserImpl<P extends Parser> extends HKT {
  fn: (
    input: Cast<this[_], ParserInput<P>>,
  ) => Parse<P, typeof input> extends infer R extends ParserResult<any, string>
    ? $Equal<R["success"], true> extends true
      ? R["data"]
      : never
    : never;
}

export interface FilterNever extends HKT {
  fn: (input: Cast<this[_], unknown[]>) => $FilterNever<typeof input>;
}

type $FilterNever<T extends unknown[], Acc extends unknown[] = []> = T extends [
  infer Head,
  ...infer Rest,
]
  ? $FilterNever<Rest, [Head] extends [never] ? Acc : [...Acc, Head]>
  : Acc;

export interface Multiply extends HKT {
  fn: (
    input: Cast<this[_], [number, number]>,
  ) => $Multiply<(typeof input)[0], (typeof input)[1]>;
}

export interface MultiplyBy extends HKT {
  fn: (input: Cast<this[_], number>) => MultiplyByImpl<typeof input>;
}

interface MultiplyByImpl<T extends number> extends HKT {
  fn: (input: Cast<this[_], number>) => $Multiply<typeof input, T>;
}

type $Multiply<
  Left extends string | number | bigint,
  Right extends string | number | bigint,
> =
  AbsWithSign<Left> extends [
    infer LeftAbs extends string | bigint | number,
    infer LeftSign extends boolean,
  ]
    ? AbsWithSign<Right> extends [
        infer RightAbs extends string | bigint | number,
        infer RightSign extends boolean,
      ]
      ? ApplySign<MultiplyInner<LeftAbs, RightAbs>, $Equal<LeftSign, RightSign>>
      : never
    : never;

type MultiplyInner<
  Left extends string | number | bigint,
  Right extends string | number | bigint,
> =
  MultiplyDigitSlices<
    $Digits<`${Left}`>,
    $Digits<`${Right}`>
  > extends infer Result extends Digit[]
    ? $ToNumber<DigitsToString<Result>>
    : never;

type MultiplyDigitSlices<
  Left extends Digit[],
  Right extends Digit[],
  Padding extends Digit[] = [],
> = Right extends []
  ? [0]
  : SumDigitSlices<
      [...MultiplyWithDigit<Left, LastOrDefault<Right, 0>>, ...Padding],
      MultiplyDigitSlices<
        Left,
        Init<Right>,
        [...Padding, 0]
      > extends infer R extends Digit[]
        ? R
        : never
    >;

type MultiplyWithDigit<
  Value extends Digit[],
  Factor extends Digit,
  Result extends Digit[] = [],
  Carry extends Digit = 0,
> = Value extends []
  ? Carry extends 0
    ? Result
    : [Carry, ...Result]
  : SumDigitSlices<
        MultiplicationTable[LastOrDefault<Value, 0>][Factor],
        [Carry]
      > extends infer Current extends readonly Digit[]
    ? Current extends [infer Carry extends Digit, infer Out extends Digit]
      ? MultiplyWithDigit<Init<Value>, Factor, [Out, ...Result], Carry>
      : MultiplyWithDigit<Init<Value>, Factor, [...Current, ...Result], 0>
    : never;

type MultiplicationTable = { [key in Digit]: MultiplicationRow<key> };

type MultiplicationRow<
  Value extends Digit,
  Accumulator extends Digit[][] = [[0]],
> = Accumulator["length"] extends 10
  ? Accumulator
  : Accumulator extends [...any[], infer Last extends Digit[]]
    ? SumDigitSlices<Last, [Value]> extends infer Result extends Digit[]
      ? MultiplicationRow<Value, [...Accumulator, Result]>
      : never
    : never;

export interface Extends extends HKT {
  fn: (input: Cast<this[_], unknown>) => ExtendsImpl<typeof input>;
}

interface ExtendsImpl<T> extends HKT {
  fn: (input: Cast<this[_], unknown>) => typeof input extends T ? true : false;
}

export interface Repeat extends HKT {
  fn: (input: Cast<this[_], number>) => RepeatImpl<typeof input>;
}

interface RepeatImpl<N extends number> extends HKT {
  fn: (input: Cast<this[_], unknown>) => $Repeat<typeof input, N>;
}

export interface Repeat_ extends HKT {
  fn: (
    input: Cast<this[_], [unknown, number]>,
  ) => $Repeat<(typeof input)[0], (typeof input)[1]>;
}

export interface InObject extends HKT {
  fn: (
    input: Cast<this[_], Record<PropertyKey, any>>,
  ) => ExtendsImpl<keyof typeof input>;
}

type $Repeat<
  T,
  N extends number,
  Acc extends unknown[] = [],
> = Acc["length"] extends N ? Acc : $Repeat<T, N, [...Acc, T]>;

export interface Join extends HKT {
  fn: (delimiter: Cast<this[_], string>) => JoinImpl<typeof delimiter>;
}

type Joinable = string | number | boolean;

interface JoinImpl<D extends string> extends HKT {
  fn: (input: Cast<this[_], Joinable[]>) => $Join<typeof input, D>;
}

type $Join<
  T extends Joinable[],
  D extends string,
  Acc extends string = "",
> = T extends [infer Head extends Joinable, ...infer Rest extends Joinable[]]
  ? $Join<Rest, D, "" extends Acc ? `${Head}` : `${Acc},${Head}`>
  : Acc;

export interface Return extends HKT {
  fn: (input: Cast<this[_], unknown>) => ReturnImpl<typeof input>;
}

interface ReturnImpl<T> extends HKT {
  fn: (_input: Cast<this[_], unknown>) => T;
}

export interface FlattenDepth extends HKT {
  fn: (input: Cast<this[_], number>) => FlattenDepthImpl<typeof input>;
}

interface FlattenDepthImpl<D extends number> extends HKT {
  fn: (input: Cast<this[_], unknown[]>) => $FlattenDepth<typeof input, D>;
}

type $FlattenDepth<
  T extends readonly unknown[],
  Depth extends number,
> = Depth extends 0
  ? T
  : T extends [infer Head, ...infer Rest]
    ? [
        ...(Head extends readonly unknown[]
          ? $FlattenDepth<Head, $<Subtract, [Depth, 1]>>
          : [Head]),
        ...$FlattenDepth<Rest, Depth>,
      ]
    : [];

export interface Push extends HKT {
  fn: (input: Cast<this[_], unknown>) => PushImpl<typeof input>;
}

interface PushImpl<T> extends HKT {
  fn: (input: Cast<this[_], unknown>) => $Push<typeof input, T>;
}

type $Push<T, U> = T extends unknown[] ? [...T, U] : [T, U];

export interface Intersect_ extends HKT {
  fn: (
    input: Cast<this[_], [unknown, unknown]>,
  ) => $Intersect<(typeof input)[0], (typeof input)[1]>;
}

type $Intersect<T, U> = {
  [Key in keyof T]: Key extends keyof U ? T[Key] & U[Key] : never;
};

export interface EvalIntersection extends HKT {
  fn: (input: Cast<this[_], unknown>) => Omit<typeof input, never>;
}

export interface MergeObjects_ extends HKT {
  fn: (
    input: Cast<
      this[_],
      [Record<PropertyKey, unknown>, Record<PropertyKey, unknown>]
    >,
  ) => $MergeObjects<(typeof input)[0], (typeof input)[1]>;
}

type $MergeObjects<
  L extends Record<PropertyKey, unknown>,
  R extends Record<PropertyKey, unknown>,
> = Omit<L, keyof R> & R;

export interface ObjectFromEntries extends HKT {
  fn: (
    input: Cast<this[_], Array<[PropertyKey, unknown]>>,
  ) => $ObjectFromEntries<typeof input>;
}

type $ObjectFromEntries<
  Entries extends Array<[PropertyKey, unknown]>,
  Acc = never,
> = Entries extends [
  infer Head extends [PropertyKey, unknown],
  ...infer Rest extends Array<[PropertyKey, unknown]>,
]
  ? $ObjectFromEntries<Rest, Acc | { [Key in Head[0]]: Head[1] }>
  : UnionToIntersection<Acc>;

type EntriesToObjects<T> = T extends [infer Key extends string, infer Value]
  ? { [K in Key]: Value }
  : never;

export interface TupleToUnion extends HKT {
  fn: (input: Cast<this[_], unknown[]>) => (typeof input)[number];
}

export interface UnionToTuple extends HKT {
  fn: (input: Cast<this[_], unknown>) => $UnionToTuple<typeof input>;
}

type $UnionToTuple<U> = [U] extends [never]
  ? []
  : LastInUnion<U> extends infer Last
    ? [...$UnionToTuple<Exclude<U, Last>>, Last]
    : never;

type UnionToIntersection<U> = (
  U extends any ? (arg: U) => any : never
) extends (arg: infer I) => void
  ? I
  : never;

type LastInUnion<U> =
  UnionToIntersection<U extends any ? (arg: U) => void : never> extends (
    arg: infer Last,
  ) => void
    ? Last
    : never;

export interface Chunk extends HKT {
  fn: (size: Cast<this[_], number>) => ChunkImpl<typeof size>;
}

interface ChunkImpl<S extends number> extends HKT {
  fn: (input: Cast<this[_], unknown[]>) => $Chunk<typeof input, S>;
}

type $Chunk<
  T extends unknown[],
  S extends number,
  Acc extends unknown[][] = [],
  Current extends unknown[] = [],
> = T extends [infer Head, ...infer Rest]
  ? $Equal<Current["length"], S> extends true
    ? $Chunk<Rest, S, [...Acc, Current], [Head]>
    : $Chunk<Rest, S, Acc, [...Current, Head]>
  : $Equal<Current["length"], 0> extends true
    ? Acc
    : [...Acc, Current];

export interface GridSearch extends HKT {
  fn: (
    input: Cast<this[_], [[number, number], HKT?]>,
  ) => GridSearchImpl<(typeof input)[0], (typeof input)[1]>;
}

interface GridSearchImpl<
  DepthLimits extends [number, number],
  Filter extends HKT | undefined = undefined,
> extends HKT {
  fn: (
    input: Cast<this[_], unknown[][]>,
  ) => $GridSearch<typeof input, DepthLimits, Filter>;
}

type $GridSearch<
  T extends unknown[][],
  DepthLimits extends [number, number],
  Filter extends HKT | undefined = undefined,
> = GridSearchHelper<T, [DepthLimits[0], $<Add, [DepthLimits[1], 1]>], Filter>;

type GridSearchHelper<
  T extends unknown[][],
  DepthLimits extends [number, number],
  Filter extends HKT | undefined = undefined,
  Position extends [number, number] = [0, 0],
  Acc extends unknown[][] = [],
> =
  GridSearchAt<
    T,
    DepthLimits,
    Filter,
    Position,
    [GetInGrid<T, Position>],
    { [Key in $<MakeVisitedKey, Position>]: true }
  > extends infer R extends unknown[][]
    ? NextPosition<T, Position> extends infer Next
      ? [Next] extends [never]
        ? Acc
        : Next extends [number, number]
          ? GridSearchHelper<T, DepthLimits, Filter, Next, [...Acc, ...R]>
          : never
      : never
    : never;

type NextPosition<T extends unknown[][], Position extends [number, number]> =
  $<Add, [Position[1], 1]> extends infer NextCol extends number
    ? $<MakeRangeCheck<0, T[0]["length"]>, NextCol> extends true
      ? [Position[0], NextCol]
      : $<Add, [Position[0], 1]> extends infer NextRow extends number
        ? $<MakeRangeCheck<0, T["length"]>, NextRow> extends true
          ? [NextRow, 0]
          : never
        : never
    : never;

type GridVisited = Record<`${number},${number}`, true>;

type GridSearchAt<
  T extends unknown[][],
  DepthLimits extends [number, number],
  Filter extends HKT | undefined,
  Position extends [number, number],
  Path extends unknown[],
  Visited extends GridVisited,
> = (
  $<MakeRangeCheck<DepthLimits[0], DepthLimits[1]>, Path["length"]> extends true
    ? Filter extends HKT
      ? $<Filter, Cast<Path, InputOf<Filter>>> extends true
        ? [Path]
        : []
      : [Path]
    : []
) extends infer Current extends unknown[][]
  ? (
      $<Compare, [Path["length"], DepthLimits[1]]> extends CompareResult.Lt
        ? GridNeighbors<T, Position, Visited> extends infer N extends Array<
            [number, number]
          >
          ? GridSearchAtRecurse<T, DepthLimits, Filter, N, Path, Visited>
          : []
        : []
    ) extends infer Recurse extends unknown[][]
    ? [...Current, ...Recurse]
    : never
  : never;

type GridSearchAtRecurse<
  T extends unknown[][],
  DepthLimits extends [number, number],
  Filter extends HKT | undefined,
  NextPositions extends Array<[number, number]>,
  Path extends unknown[] = [],
  Visited extends GridVisited = {},
  Acc extends unknown[][] = [],
> = NextPositions extends [
  infer Head extends [number, number],
  ...infer Rest extends Array<[number, number]>,
]
  ? GridSearchAt<
      T,
      DepthLimits,
      Filter,
      Head,
      [...Path, GetInGrid<T, Head>],
      Visited & { [Key in $<MakeVisitedKey, Head>]: true }
    > extends infer R extends unknown[][]
    ? GridSearchAtRecurse<
        T,
        DepthLimits,
        Filter,
        Rest,
        Path,
        Visited,
        [...Acc, ...R]
      >
    : never
  : Acc;

type GetInGrid<
  T extends unknown[][],
  Position extends [number, number],
> = Position[0] extends keyof T
  ? Position[1] extends keyof T[Position[0]]
    ? T[Position[0]][Position[1]]
    : never
  : never;

type NeighborDeltas = $<
  $<
    Chain,
    [
      $<Repeat, 2>,
      CartesianProduct,
      $<Filter, $<Chain, [$<All, $<Equal, 0>>, Not]>>,
    ]
  >,
  $<Range, [-1, 2]>
>;

type MakeRangeCheck<L extends number, R extends number> = $<
  Chain,
  [$<ApplyMany, [$<GreaterThanOrEquals, L>, $<LessThan, R>]>, $<All, Identity>]
>;

type MakeVisitedKey = $<Join, ",">;

type GridNeighbors<
  T extends unknown[][],
  Position extends [number, number],
  Visited extends GridVisited,
> = $<
  $<
    Chain,
    [
      // Compute new positions
      CartesianProduct,
      $<MapWith, $<Chain, [Transpose, $<MapWith, Add>]>>,
      // Remove positions out of bounds
      $<
        Filter,
        $<
          Chain,
          [
            $<
              ApplyMany,
              [
                $<Chain, [$<At, 0>, MakeRangeCheck<0, T["length"]>]>,
                $<Chain, [$<At, 1>, MakeRangeCheck<0, T[0]["length"]>]>,
              ]
            >,
            $<All, Identity>,
          ]
        >
      >,
      // Remove visited positions
      $<Filter, $<Chain, [MakeVisitedKey, $<InObject, Visited>, Not]>>,
    ]
  >,
  [[Position], NeighborDeltas]
>;

export interface FlipObject extends HKT {
  fn: (
    input: Cast<this[_], Record<PropertyKey, PropertyKey>>,
  ) => $FlipObject<typeof input>;
}

type $FlipObject<T extends Record<PropertyKey, PropertyKey>> = {
  [P in keyof T as T[P] extends PropertyKey ? T[P] : string]: P extends keyof T
    ? P
    : never;
};

export interface MapObjectValues extends HKT {
  fn: (input: Cast<this[_], HKT>) => MapObjectValuesImpl<typeof input>;
}

interface MapObjectValuesImpl<Op extends HKT> extends HKT {
  fn: (
    input: Cast<this[_], Record<PropertyKey, unknown>>,
  ) => $MapObjectValues<typeof input, Op>;
}

type $MapObjectValues<
  T extends Record<PropertyKey, unknown>,
  Op extends HKT,
> = {
  [Key in keyof T]: $<Op, Cast<T[Key], InputOf<Op>>>;
};

export interface LookupObject extends HKT {
  fn: (
    input: Cast<this[_], Record<PropertyKey, unknown>>,
  ) => LookupObjectImpl<typeof input>;
}

interface LookupObjectImpl<T extends Record<PropertyKey, unknown>> extends HKT {
  fn: (
    input: Cast<this[_], PropertyKey>,
  ) => typeof input extends keyof T ? T[typeof input] : never;
}

export interface TopologicalSort extends HKT {
  fn: (input: Cast<this[_], Graph>) => $TopologicalSort<typeof input>;
}

type GraphNode = PropertyKey;

type GraphEdges = [GraphNode, GraphNode[]];

type Graph = Array<GraphEdges>;

type $TopologicalSort<G extends Graph> =
  CalculateInDegrees<G> extends infer InDegrees extends Record<
    GraphNode,
    number
  >
    ? TopologicalSortHelper<G, InDegrees>
    : never;

type TopologicalSortHelper<
  G extends Graph,
  InDegrees extends Record<GraphNode, number>,
  Used extends GraphNode = never,
  Acc extends GraphNode[] = [],
> =
  TopologicalSortNext<G, InDegrees, Used> extends infer N
    ? [N] extends [never]
      ? Acc
      : N extends PropertyKey
        ? TopologicalSortHelper<
            G,
            TopologicalSortRemove<G, InDegrees, N>,
            Used | N,
            [...Acc, N]
          >
        : never
    : never;

type CalculateInDegrees<
  G extends Graph,
  Acc extends Record<GraphNode, number> = {},
> = G extends [infer Head extends GraphEdges, ...infer Rest extends Graph]
  ? CalculateInDegrees<Rest, InDegreesHelper<Head[1], Acc, 1>>
  : Acc;

type InDegreesHelper<
  To extends GraphNode[],
  Acc extends Record<GraphNode, number>,
  Delta extends number,
> = To extends [infer Head extends GraphNode, ...infer Rest extends GraphNode[]]
  ? InDegreesHelper<
      Rest,
      Omit<Acc, Head> & {
        [Key in Head]: $<Add, [Head extends keyof Acc ? Acc[Head] : 0, Delta]>;
      },
      Delta
    >
  : Acc;

type TopologicalSortNext<
  G extends Graph,
  InDegrees extends Record<GraphNode, number>,
  Used extends GraphNode,
> = G extends [
  infer Head extends GraphEdges,
  ...infer Rest extends GraphEdges[],
]
  ? Head[0] extends Used
    ? TopologicalSortNext<Rest, InDegrees, Used>
    : Head[0] extends keyof InDegrees
      ? $Equal<InDegrees[Head[0]], 0> extends true
        ? Head[0]
        : TopologicalSortNext<Rest, InDegrees, Used>
      : Head[0]
  : never;

type TopologicalSortRemove<
  G extends Graph,
  InDegrees extends Record<GraphNode, number>,
  N extends GraphNode,
> =
  $<$<Filter, $<Chain, [$<At, 0>, $<Equal, N>]>>, G> extends [
    infer E extends GraphEdges,
  ]
    ? InDegreesHelper<E[1], InDegrees, -1>
    : never;

export interface FindPositions2D extends HKT {
  fn: (input: Cast<this[_], HKT>) => FindPositions2DImpl<typeof input>;
}

interface FindPositions2DImpl<Op extends HKT> extends HKT {
  fn: (input: Cast<this[_], unknown[][]>) => $FindPositions2D<typeof input, Op>;
}

type $FindPositions2D<T extends unknown[][], Op extends HKT> = $<
  $<Chain, [$<MapWith, UnionToTuple>, $<FlattenDepth, 1>]>,
  {
    [Row in keyof T]: {
      [Col in keyof T[Row] & string as $<
        Op,
        Cast<T[Row][Col], InputOf<Op>>
      > extends true
        ? Col
        : never]: $<$<MapWith, ToNumber>, [`${Row}`, `${Col}`]>;
    } extends infer R
      ? R[keyof R]
      : never;
  }
>;

export interface BooleanToNumber extends HKT {
  fn: (
    input: Cast<this[_], boolean>,
  ) => $Equal<typeof input, true> extends true ? 1 : 0;
}
