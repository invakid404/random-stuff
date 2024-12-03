import { $, ToNumber } from "./lib.js";

declare const _: unique symbol;

type _ = typeof _;

type ParserFn = (...args: never[]) => unknown;

export type ParserInput<T extends Parser> = T extends ParserHKT
  ? ParserInputHelper<T>
  : any;

type ParserInputHelper<T extends ParserHKT> = Parameters<T["fn"]>[0];

export type ParserResult<Data = never, Rest extends string = string> = Omit<
  ([Data] extends [never]
    ? { success: false }
    : {
        success: true;
        data: Data;
      }) & { rest: Rest },
  never
>;

abstract class ParserHKT {
  [_]: unknown;
  abstract fn: ParserFn;
}

export interface Just extends ParserHKT {
  fn: (item: Cast<this[_], string>) => JustImpl<typeof item>;
}

interface JustImpl<Item extends string> extends ParserHKT {
  fn: (
    data: Cast<this[_], string>,
  ) => typeof data extends `${infer Data extends Item}${infer Rest}`
    ? ParserResult<Data, Rest>
    : ParserResult<never, typeof data>;
}

export interface NoneOf extends ParserHKT {
  fn: (chars: Cast<this[_], string>) => NoneOfImpl<typeof chars>;
}

interface NoneOfImpl<Chars extends string> extends ParserHKT {
  fn: (
    data: Cast<this[_], string>,
  ) => typeof data extends `${infer Head}${infer Rest}`
    ? Head extends Chars
      ? ParserResult<never, typeof data>
      : ParserResult<Head, Rest>
    : ParserResult<never, typeof data>;
}

export interface EOF extends ParserHKT {
  fn: (
    data: Cast<this[_], string>,
  ) => "" extends typeof data
    ? ParserResult<"", typeof data>
    : ParserResult<never, typeof data>;
}

export type MaybeResult<Data = unknown> =
  | { success: true; data: Data }
  | { success: false };

export interface Maybe extends ParserHKT {
  fn: (parser: Cast<this[_], Parser>) => MaybeImpl<typeof parser>;
}

interface MaybeImpl<P extends Parser> extends ParserHKT {
  fn: (
    data: Cast<this[_], ParserInput<P>>,
  ) => Parse<P, typeof data> extends infer R extends ParserResult<
    unknown,
    string
  >
    ? true extends R["success"]
      ? ParserResult<{ success: true; data: R["data"] }, R["rest"]>
      : ParserResult<{ success: false }, typeof data>
    : never;
}

export interface Left extends ParserHKT {
  fn: (
    parsers: Cast<this[_], [Parser, Parser]>,
  ) => LeftImpl<(typeof parsers)[0], (typeof parsers)[1]>;
}

interface LeftImpl<L extends Parser, R extends Parser> extends ParserHKT {
  fn: (
    data: Cast<this[_], ParserInput<L>>,
  ) => $Pair<L, R, typeof data> extends ParserResult<
    infer Data extends [unknown, unknown],
    infer Rest
  >
    ? ParserResult<Data[0], Rest>
    : never;
}

export interface Right extends ParserHKT {
  fn: (
    parsers: Cast<this[_], [Parser, Parser]>,
  ) => RightImpl<(typeof parsers)[0], (typeof parsers)[1]>;
}

interface RightImpl<L extends Parser, R extends Parser> extends ParserHKT {
  fn: (
    data: Cast<this[_], ParserInput<L>>,
  ) => $Pair<L, R, typeof data> extends ParserResult<
    infer Data extends [unknown, unknown],
    infer Rest
  >
    ? ParserResult<Data[1], Rest>
    : never;
}

export interface Pair extends ParserHKT {
  fn: (
    parsers: Cast<this[_], [Parser, Parser]>,
  ) => PairImpl<(typeof parsers)[0], (typeof parsers)[1]>;
}

interface PairImpl<L extends Parser, R extends Parser> extends ParserHKT {
  fn: (data: Cast<this[_], ParserInput<L>>) => $Pair<L, R, typeof data>;
}

type $Pair<L extends Parser, R extends Parser, Data extends ParserInput<L>> =
  Parse<L, Data> extends infer LRes extends ParserResult<unknown, string>
    ? false extends LRes["success"]
      ? ParserResult<never, Data>
      : Parse<R, Cast<LRes["rest"], ParserInput<R>>> extends infer RRes extends
            ParserResult<unknown, string>
        ? false extends RRes["success"]
          ? ParserResult<never, Data>
          : ParserResult<[LRes["data"], RRes["data"]], RRes["rest"]>
        : never
    : never;

export interface Seq extends ParserHKT {
  fn: (parsers: Cast<this[_], Array<Parser>>) => SeqImpl<typeof parsers>;
}

interface SeqImpl<P extends Array<Parser>> extends ParserHKT {
  fn: (data: Cast<this[_], ParserInput<P[number]>>) => $Seq<P, typeof data>;
}

type $Seq<
  P extends Array<Parser>,
  Data extends ParserInput<P[number]>,
  OriginalData extends ParserInput<P[number]> = Data,
  Acc extends unknown[] = [],
> = P extends [infer Head extends Parser, ...infer Rest extends Array<Parser>]
  ? Parse<Head, Data> extends infer R extends ParserResult<unknown, string>
    ? false extends R["success"]
      ? ParserResult<never, OriginalData>
      : $Seq<
          Rest,
          Cast<R["rest"], ParserInput<P[number]>>,
          OriginalData,
          [...Acc, R["data"]]
        >
    : never
  : ParserResult<Acc, Data>;

export interface Choice extends ParserHKT {
  fn: (parsers: Cast<this[_], Array<Parser>>) => ChoiceImpl<typeof parsers>;
}

interface ChoiceImpl<P extends Array<Parser>> extends ParserHKT {
  fn: (data: Cast<this[_], ParserInput<P[number]>>) => $Choice<P, typeof data>;
}

type $Choice<
  P extends Array<Parser>,
  Data extends ParserInput<P[number]>,
> = P extends [infer Head extends Parser, ...infer Rest extends Array<Parser>]
  ? Parse<Head, Data> extends infer R extends ParserResult<unknown, string>
    ? true extends R["success"]
      ? R
      : $Choice<Rest, Data>
    : never
  : ParserResult<never, Data>;

export interface Many0 extends ParserHKT {
  fn: (parser: Cast<this[_], Parser>) => Many0Impl<typeof parser>;
}

interface Many0Impl<P extends Parser> extends ParserHKT {
  fn: (data: Cast<this[_], ParserInput<P>>) => $Many0<P, typeof data>;
}

type $Many0<
  P extends Parser,
  Data extends ParserInput<P>,
  Acc extends readonly unknown[] = [],
> =
  Parse<P, Data> extends infer R extends ParserResult<unknown, string>
    ? true extends R["success"]
      ? $Many0<P, Cast<R["rest"], ParserInput<P>>, [...Acc, R["data"]]>
      : ParserResult<Acc, Data>
    : never;

export interface Many1 extends ParserHKT {
  fn: (parser: Cast<this[_], Parser>) => Many1Impl<typeof parser>;
}

interface Many1Impl<P extends Parser> extends ParserHKT {
  fn: (data: Cast<this[_], ParserInput<P>>) => $Many1<P, typeof data>;
}

type $Many1<P extends Parser, Data extends ParserInput<P>> =
  $Many0<P, Data> extends infer R extends ParserResult<unknown[], string>
    ? [] extends R["data"]
      ? ParserResult<never, Data>
      : R
    : never;

export interface SepBy0 extends ParserHKT {
  fn: (
    input: Cast<this[_], [Parser, Parser]>,
  ) => SepBy0Impl<(typeof input)[0], (typeof input)[1]>;
}

interface SepBy0Impl<P extends Parser, Sep extends Parser> extends ParserHKT {
  fn: (data: Cast<this[_], ParserInput<P>>) => $SepBy0<P, Sep, typeof data>;
}

type $SepBy0<
  P extends Parser,
  Sep extends Parser,
  Data extends ParserInput<P>,
> =
  Parse<P, Data> extends infer R extends ParserResult<unknown, string>
    ? false extends R["success"]
      ? ParserResult<[], Data>
      : Parse<Right, [Parse<Many0, Sep>, P]> extends infer Rec extends ParserHKT
        ? SepBy0Recurse<
            Rec,
            Cast<R["rest"], ParserInput<Rec>>
          > extends infer RR extends ParserResult<unknown[], string>
          ? ParserResult<[R["data"], ...RR["data"]], RR["rest"]>
          : never
        : never
    : never;

type SepBy0Recurse<
  P extends ParserHKT,
  Data extends ParserInput<P>,
  Acc extends unknown[] = [],
> =
  Parse<P, Data> extends infer R extends ParserResult<unknown, string>
    ? false extends R["success"]
      ? ParserResult<Acc, Data>
      : SepBy0Recurse<P, Cast<R["rest"], ParserInput<P>>, [...Acc, R["data"]]>
    : ParserResult<Acc, Data>;

export interface SepBy1 extends ParserHKT {
  fn: (
    input: Cast<this[_], [Parser, Parser]>,
  ) => SepBy1Impl<(typeof input)[0], (typeof input)[1]>;
}

interface SepBy1Impl<P extends Parser, Sep extends Parser> extends ParserHKT {
  fn: (data: Cast<this[_], ParserInput<P>>) => $SepBy1<P, Sep, typeof data>;
}

type $SepBy1<
  P extends Parser,
  Sep extends Parser,
  Data extends ParserInput<P>,
> =
  $SepBy0<P, Sep, Data> extends infer R extends ParserResult<unknown, string>
    ? [] extends R["data"]
      ? ParserResult<never, Data>
      : R
    : never;

export interface TryParseAll extends ParserHKT {
  fn: (data: Cast<this[_], Parser>) => TryParseAllImpl<typeof data>;
}

interface TryParseAllImpl<P extends Parser> extends ParserHKT {
  fn: (data: Cast<this[_], string>) => $TryParseAll<typeof data, P>;
}

type $TryParseAll<
  T extends string,
  P extends Parser,
  Acc extends unknown[] = [],
> = "" extends T
  ? ParserResult<Acc, T>
  : Parse<P, Cast<T, ParserInput<P>>> extends infer R extends ParserResult<
        any,
        string
      >
    ? R["success"] extends true
      ? $TryParseAll<R["rest"], P, [...Acc, R["data"]]>
      : T extends `${string}${infer Rest}`
        ? $TryParseAll<Rest, P, Acc>
        : ParserResult<Acc, T>
    : never;

export abstract class Mapper {
  data: unknown;
  abstract map: (data: never) => unknown;
}

export interface MapResult extends ParserHKT {
  fn: (
    input: Cast<this[_], [Parser, ...Mapper[]]>,
  ) => typeof input extends [
    infer P extends Parser,
    ...infer M extends Mapper[],
  ]
    ? MapResultImpl<P, M>
    : never;
}

interface MapResultImpl<P extends Parser, M extends Mapper[]>
  extends ParserHKT {
  fn: (
    input: Cast<this[_], ParserInput<P>>,
  ) => Parse<P, typeof input> extends infer R extends ParserResult<
    unknown,
    string
  >
    ? false extends R["success"]
      ? R
      : ParserResult<$MapResult<M, R["data"]>, R["rest"]>
    : never;
}

type $MapResult<M extends Mapper[], Data> = M extends [
  infer Head extends Mapper,
  ...infer Rest extends Mapper[],
]
  ? $MapResult<Rest, ReturnType<(Head & { data: Data })["map"]>>
  : Data;

type Lazy<P = any> = () => P;

type Eval<P extends Parser> = P extends Lazy
  ? ReturnType<P> extends infer T extends ParserHKT
    ? T
    : never
  : P extends ParserHKT
    ? P
    : never;

type Cast<T, U> = T extends U ? T : U;

export type Parse<P extends Parser, Data extends ParserInput<P>> = Eval<P> & {
  [_]: Data;
} extends infer T extends ParserHKT
  ? ReturnType<T["fn"]> extends infer R
    ? R
    : never
  : never;

export type Parser = ParserHKT | Lazy;

export interface UnwrapMaybe extends Mapper {
  map: (
    data: this["data"],
  ) => typeof data extends unknown[] ? $UnwrapMaybe<typeof data> : never;
}

type $UnwrapMaybe<T extends unknown[], Acc extends unknown[] = []> = T extends [
  infer Head,
  ...infer Rest,
]
  ? $UnwrapMaybe<
      Rest,
      Head extends MaybeResult
        ? Head extends { success: true; data: infer Data }
          ? [...Acc, Data]
          : Acc
        : [...Acc, Head]
    >
  : Acc;

export interface Flatten extends Mapper {
  map: (
    data: this["data"],
  ) => typeof data extends unknown[] ? $Flatten<typeof data> : never;
}

type $Flatten<Array extends readonly any[]> = Array extends [
  infer Head,
  ...infer Rest,
]
  ? Head extends readonly any[]
    ? [...$Flatten<Head>, ...$Flatten<Rest>]
    : [Head, ...$Flatten<Rest>]
  : [];

export interface Join extends Mapper {
  map: (
    data: this["data"],
  ) => typeof data extends string[] ? $Join<typeof data> : never;
}

type $Join<T extends string[], Acc extends string = ""> = T extends [
  infer Head extends string,
  ...infer Rest extends string[],
]
  ? $Join<Rest, `${Acc}${Head}`>
  : Acc;

export interface StringToNumber extends Mapper {
  map: (
    data: this["data"],
  ) => typeof data extends string ? $<ToNumber, typeof data> : never;
}

type Digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";

export type Digits0 = Parse<Many0, Parse<Just, Digit>>;
export type Digits1 = Parse<Many1, Parse<Just, Digit>>;

export type NumberParser = Parse<
  MapResult,
  [
    Parse<
      Seq,
      [
        Parse<Maybe, Parse<Just, "-">>,
        Parse<
          Choice,
          [
            Parse<Just, "0">,
            Parse<Pair, [Parse<Just, Exclude<Digit, "0">>, Digits0]>,
          ]
        >,
        Parse<Maybe, Parse<Pair, [Parse<Just, ".">, Digits1]>>,
      ]
    >,
    UnwrapMaybe,
    Flatten,
    Join,
    StringToNumber,
  ]
>;

export type Between<
  L extends Parser,
  R extends Parser,
  P extends Parser,
> = Parse<Left, [Parse<Right, [L, P]>, R]>;

export type Str<
  T extends string,
  Acc extends Parser[] = [],
> = T extends `${infer Head}${infer Rest}`
  ? Str<Rest, [...Acc, Parse<Just, Head>]>
  : Parse<MapResult, [Parse<Seq, Acc>, Join]>;

export interface ToLiteral<T> extends Mapper {
  map: () => T;
}
