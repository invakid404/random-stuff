import {
  $,
  Add,
  AddTo,
  Any,
  Apply_,
  ApplyMany,
  At,
  AtDeep,
  BooleanToNumber,
  CartesianProduct,
  Chain,
  Equal,
  Filter,
  FindPositions2D,
  Flip,
  HKT,
  Identity,
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

type FindStartPositions<T extends string> = $<
  Chain,
  [$<FindPositions2D, $<Equal, T>>]
>;

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
      [$<Chain, [$<At, 1>, ComputePaths, $<Flip, Filter>]>, $<At, 0>]
    >,
    Apply_,
    Length,
  ]
>;

type PrepareGridForChecks<Condition extends HKT> = $<
  Chain,
  [$<Flip, AtDeep>, MapWith, $<Push, Condition>, Chain]
>;

type IsValidXmasPath = $<Equal, $<Tail, Xmas>>;

export type Part1<Input extends string> = $<
  $<
    Chain,
    [
      Parser,
      $<
        ApplyMany,
        [
          $<Chain, [PrepareGridForChecks<IsValidXmasPath>, ToArray]>,
          FindStartPositions<Xmas["0"]>,
        ]
      >,
      CartesianProduct,
      $<MapReduce, [CheckPathsAt, Add, 0]>,
    ]
  >,
  Input
>;

type XPattern = $<MakeComputePath, [[-1, -1], [-1, 1], [1, -1], [1, 1]]>;

type IsValidXPattern = $<
  $<Chain, [$<MapWith, Equal>, ApplyMany, $<Push, $<Any, Identity>>, Chain]>,
  [
    ["M", "S", "M", "S"],
    ["S", "M", "S", "M"],
    ["M", "M", "S", "S"],
    ["S", "S", "M", "M"],
  ]
>;

type CheckXPatternAt = $<
  Chain,
  [
    $<ApplyMany, [$<At, 0>, $<Chain, [$<At, 1>, XPattern]>]>,
    Apply_,
    BooleanToNumber,
  ]
>;

export type Part2<Input extends string> = $<
  $<
    Chain,
    [
      Parser,
      $<
        ApplyMany,
        [
          $<Chain, [PrepareGridForChecks<IsValidXPattern>, ToArray]>,
          FindStartPositions<"A">,
        ]
      >,
      CartesianProduct,
      $<MapReduce, [CheckXPatternAt, Add, 0]>,
    ]
  >,
  Input
>;
