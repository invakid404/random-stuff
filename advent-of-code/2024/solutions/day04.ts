import {
  $,
  Add,
  AddTo,
  Apply_,
  ApplyMany,
  At,
  AtDeep,
  CartesianProduct,
  Chain,
  Equal,
  Filter,
  FindPositions2D,
  Flip,
  Length,
  MapReduce,
  MapWith,
  MultiplyBy,
  Push,
  Range,
  Return,
  SplitBy,
  Tail,
  ToArray,
  Transpose,
} from "../lib/lib.js";

type Parser = $<Chain, [$<SplitBy, "\n">, $<MapWith, $<SplitBy, "">>]>;

type Xmas = ["X", "M", "A", "S"];

type FindStartPositions = $<Chain, [$<FindPositions2D, $<Equal, Xmas["0"]>>]>;

type ComputePaths = $<
  $<
    Chain,
    [
      $<
        ApplyMany,
        [
          $<
            Chain,
            [
              $<Return, $<Range, [1, Xmas["length"]]>>,
              $<MapWith, $<Chain, [MultiplyBy, MapWith, MapWith]>>,
            ]
          >,
          ToArray,
        ]
      >,
      CartesianProduct,
      $<MapWith, Apply_>,
      Transpose,
      $<MapWith, MakeComputePath>,
      ApplyMany,
    ]
  >,
  [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]]
>;

type MakeComputePath = $<
  Chain,
  [
    $<
      MapWith,
      $<
        Chain,
        [
          $<
            ApplyMany,
            [$<Chain, [$<Return, [0, 1]>, $<MapWith, At>]>, $<MapWith, AddTo>]
          >,
          Transpose,
          $<MapWith, Chain>,
          ApplyMany,
        ]
      >
    >,
    ApplyMany,
  ]
>;

type CheckPathsAt = $<
  Chain,
  [
    $<
      ApplyMany,
      [
        $<Chain, [$<At, 1>, ComputePaths, $<Flip, Filter>]>,
        $<Chain, [$<At, 0>]>,
      ]
    >,
    Apply_,
    Length,
  ]
>;

type PrepareGridForChecks = $<
  Chain,
  [$<Flip, AtDeep>, MapWith, $<Push, $<Equal, $<Tail, Xmas>>>, Chain]
>;

export type Part1<Input extends string> = $<
  $<
    Chain,
    [
      Parser,
      $<
        ApplyMany,
        [$<Chain, [PrepareGridForChecks, ToArray]>, FindStartPositions]
      >,
      CartesianProduct,
      $<MapReduce, [CheckPathsAt, Add, 0]>,
    ]
  >,
  Input
>;
