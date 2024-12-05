import {
  $,
  Add,
  Apply_,
  ApplyMany,
  At,
  CartesianProduct,
  Chain,
  DiffObjects_,
  Equal,
  Equal_,
  EvalIntersection,
  Filter,
  FlipObject,
  Identity,
  Intersect_,
  LookupObject,
  MapObjectValues,
  MapReduce,
  MapWith,
  MergeObjects_,
  Middle,
  Not,
  ObjectFromEntries,
  Push,
  Return,
  Reverse,
  SplitBy,
  Tails,
  ToArray,
  ToNumber,
  TopologicalSort,
  TupleToUnion,
  UnionToTuple,
  Zip,
} from "../lib/lib.js";

type RulesParser = $<
  Chain,
  [
    $<SplitBy, "\n">,
    $<MapWith, $<Chain, [$<SplitBy, "|">, $<MapWith, ToNumber>]>>,
  ]
>;

type UpdatesParser = $<
  Chain,
  [
    $<SplitBy, "\n">,
    $<MapWith, $<Chain, [$<SplitBy, ",">, $<MapWith, ToNumber>]>>,
  ]
>;

type Parser = $<
  Chain,
  [
    $<SplitBy, "\n\n">,
    $<
      ApplyMany,
      [$<Chain, [$<At, 0>, RulesParser]>, $<Chain, [$<At, 1>, UpdatesParser]>]
    >,
  ]
>;

type CollectRulesPerPage = $<
  Chain,
  [
    $<ApplyMany, [$<Chain, [$<At, 0>, ToArray]>, $<At, 1>]>,
    CartesianProduct,
    $<
      MapWith,
      $<
        Chain,
        [
          $<
            ApplyMany,
            [
              $<At, 1>,
              $<
                Chain,
                [
                  $<
                    ApplyMany,
                    [
                      $<
                        Chain,
                        [
                          $<At, 1>,
                          Equal,
                          $<Push, $<At, 0>>,
                          Reverse,
                          Chain,
                          Filter,
                        ]
                      >,
                      $<At, 0>,
                    ]
                  >,
                  Apply_,
                  $<MapWith, $<At, 1>>,
                  TupleToUnion,
                ]
              >,
            ]
          >,
        ]
      >
    >,
    ObjectFromEntries,
    EvalIntersection,
  ]
>;

type MakeOrderMap = $<
  Chain,
  [
    $<ApplyMany, [Identity, $<Chain, [Tails, $<MapWith, TupleToUnion>]>]>,
    Zip,
    ObjectFromEntries,
    EvalIntersection,
  ]
>;

type DiffUpdate = $<
  Chain,
  [
    $<ApplyMany, [$<Chain, [$<At, 1>, MakeOrderMap]>, CollectRulesPerPage]>,
    $<ApplyMany, [$<Chain, [Intersect_, EvalIntersection]>, $<At, 0>]>,
    DiffObjects_,
    EvalIntersection,
  ]
>;

type PrepareForDiffing = $<
  Chain,
  [$<ApplyMany, [$<Chain, [$<At, 0>, ToArray]>, $<At, 1>]>, CartesianProduct]
>;

type DiffUpdates = $<Chain, [PrepareForDiffing, $<MapWith, DiffUpdate>]>;

type FindCorrectUpdates = $<
  Chain,
  [
    PrepareForDiffing,
    $<Filter, $<Chain, [DiffUpdate, $<Equal, {}>]>>,
    $<MapWith, $<At, 1>>,
  ]
>;

type SumUpdates = $<MapReduce, [Middle, Add, 0]>;

export type Part1<Input extends string> = $<
  $<Chain, [Parser, FindCorrectUpdates, SumUpdates]>,
  Input
>;

type BuildEdgesObjectFromDiff = $<
  Chain,
  [
    $<
      ApplyMany,
      [
        $<
          Chain,
          [
            $<At, 0>,
            $<MapWith, $<ApplyMany, [Identity, $<Return, []>]>>,
            ObjectFromEntries,
          ]
        >,
        $<Chain, [$<At, 1>, FlipObject, $<MapObjectValues, UnionToTuple>]>,
      ]
    >,
    MergeObjects_,
  ]
>;

type BuildAdjacencyListFromDiff = $<
  Chain,
  [
    $<
      ApplyMany,
      [$<Chain, [BuildEdgesObjectFromDiff, LookupObject, ToArray]>, $<At, 0>]
    >,
    CartesianProduct,
    $<MapWith, $<ApplyMany, [$<At, 1>, Apply_]>>,
  ]
>;

export type Part2<Input extends string> = $<
  $<
    Chain,
    [
      Parser,
      $<ApplyMany, [$<At, 1>, DiffUpdates]>,
      Zip,
      $<Filter, $<Chain, [$<At, 1>, $<Equal, {}>, Not]>>,
      $<MapWith, $<Chain, [BuildAdjacencyListFromDiff, TopologicalSort]>>,
      SumUpdates,
    ]
  >,
  Input
>;
