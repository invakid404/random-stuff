import {
  $,
  Add,
  Chain,
  Equal,
  Extends,
  Filter,
  MapWith,
  Multiply,
  MustRunParser,
  Reduce,
} from "../lib/lib.js";
import {
  Between,
  Choice,
  Just,
  Left,
  Mapper,
  MapResult,
  NumberParser,
  Pair,
  Parse,
  Right,
  Str,
  TryParseAll,
} from "../lib/parser.js";

type MulParser = Parse<
  Right,
  [
    Str<"mul">,
    Between<
      Parse<Just, "(">,
      Parse<Just, ")">,
      Parse<Pair, [Parse<Left, [NumberParser, Parse<Just, ",">]>, NumberParser]>
    >,
  ]
>;

type DoInst = "do()";
type DontInst = "don't()";

type DoParser = Str<DoInst>;
type DontParser = Str<DontInst>;

type InstructionParser = Parse<
  TryParseAll,
  Parse<Choice, [MulParser, DoParser, DontParser]>
>;

export type Part1<Input extends string> = $<
  $<
    Chain,
    [
      $<MustRunParser, InstructionParser>,
      $<Filter, $<Extends, [number, number]>>,
      $<MapWith, Multiply>,
      $<Reduce, [Add, 0]>,
    ]
  >,
  Input
>;

interface FilterDisabled extends Mapper {
  map: (
    data: this["data"],
  ) => typeof data extends unknown[] ? $FilterDisabled<typeof data> : never;
}

type IsDoInst = $<Equal, DoInst>;
type IsDontInst = $<Equal, DontInst>;

type $FilterDisabled<
  T extends unknown[],
  Acc extends unknown[] = [],
  Enabled = true,
> = T extends [infer Head, ...infer Rest]
  ? $<IsDoInst, Head> extends true
    ? $FilterDisabled<Rest, Acc, true>
    : $<IsDontInst, Head> extends true
      ? $FilterDisabled<Rest, Acc, false>
      : $FilterDisabled<
          Rest,
          Enabled extends true ? [...Acc, Head] : Acc,
          Enabled
        >
  : Acc;

type EnabledInstructionParser = Parse<
  MapResult,
  [InstructionParser, FilterDisabled]
>;

export type Part2<Input extends string> = $<
  $<
    Chain,
    [
      $<MustRunParser, EnabledInstructionParser>,
      $<MapWith, Multiply>,
      $<Reduce, [Add, 0]>,
    ]
  >,
  Input
>;
