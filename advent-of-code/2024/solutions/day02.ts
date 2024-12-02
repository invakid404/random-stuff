import {
  $,
  All,
  And,
  Any,
  At,
  CartesianProduct,
  Chain,
  DropAt_,
  Filter,
  GreaterThan,
  Identity,
  Length,
  LessThan,
  Lift,
  MapWith,
  RangeFrom,
  SplitBy,
  Subtract,
  ToArray,
  ToNumber,
  Windows,
} from "../lib/lib.js";

type Parser = $<
  Chain,
  [
    $<SplitBy, "\n">,
    $<MapWith, $<Chain, [$<SplitBy, " ">, $<MapWith, ToNumber>]>>,
  ]
>;

type IsSafe = $<
  Chain,
  [
    $<Windows, 2>,
    $<MapWith, Subtract>,
    $<
      Lift,
      [
        $<All, $<And, [$<GreaterThan, 0>, $<LessThan, 4>]>>,
        $<All, $<And, [$<LessThan, 0>, $<GreaterThan, -4>]>>,
      ]
    >,
    $<Any, Identity>,
  ]
>;

export type Part1<Input extends string> = $<
  $<Chain, [Parser, $<Filter, IsSafe>, Length]>,
  Input
>;

type IsSafeAfterRemoval = $<
  Chain,
  [
    $<Lift, [ToArray, $<Chain, [Length, $<RangeFrom, 0>]>]>,
    CartesianProduct,
    $<MapWith, DropAt_>,
    $<Any, IsSafe>,
  ]
>;

export type Part2<Input extends string> = $<
  $<Chain, [Parser, $<Filter, IsSafeAfterRemoval>, Length]>,
  Input
>;
