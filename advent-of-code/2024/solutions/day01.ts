import {
  $,
  Abs,
  Add,
  Chain,
  Equal,
  Filter,
  Flip,
  MapWith,
  Reduce,
  Sort,
  SplitBy,
  Subtract,
  ToNumber,
  Transpose,
} from "../lib/lib.js";

type Parser = $<
  Chain,
  [
    $<SplitBy, "\n">,
    $<MapWith, $<SplitBy, " ">>,
    Transpose,
    $<MapWith, $<MapWith, ToNumber>>,
  ]
>;

export type Part1<Input extends string> = $<
  $<
    Chain,
    [
      Parser,
      $<MapWith, Sort>,
      Transpose,
      $<MapWith, Subtract>,
      $<MapWith, Abs>,
      $<Reduce, [Add, 0]>,
    ]
  >,
  Input
>;

export type Part2<Input extends string> =
  $<Parser, Input> extends [
    infer Left extends number[],
    infer Right extends number[],
  ]
    ? $<
        $<
          Chain,
          [
            $<MapWith, $<Chain, [Equal, $<$<Flip, Filter>, Right>]>>,
            $<MapWith, $<Reduce, [Add, 0]>>,
            $<Reduce, [Add, 0]>,
          ]
        >,
        Left
      >
    : never;
