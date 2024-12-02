import {
  $,
  Abs,
  Add,
  Apply,
  At,
  CartesianProduct,
  Chain,
  Equal,
  Filter,
  Flip,
  Lift,
  MapWith,
  Reduce,
  Sort,
  SplitBy,
  Subtract,
  ToArray,
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

export type Part2<Input extends string> = $<
  $<
    Chain,
    [
      Parser,
      $<
        Lift,
        [
          $<Chain, [$<At, 1>, $<Flip, Filter>, ToArray]>,
          $<Chain, [$<At, 0>, $<MapWith, Equal>]>,
        ]
      >,
      CartesianProduct,
      $<MapWith, $<Chain, [Apply, $<Reduce, [Add, 0]>]>>,
      $<Reduce, [Add, 0]>,
    ]
  >,
  Input
>;
