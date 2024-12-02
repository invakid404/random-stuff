import {
  $,
  All,
  And,
  Any,
  Chain,
  Filter,
  GreaterThan,
  Identity,
  Length,
  LessThan,
  Lift,
  MapWith,
  SplitBy,
  Subtract,
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

export type Part2<Input extends string> = never;
