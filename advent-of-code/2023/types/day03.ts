import {
  Max,
  Min,
  SmallerThanOrEqual,
  Split,
  Subtract,
  Sum,
  Trim,
  ObjectEntries,
  LastInUnion,
  Range,
  UnionToIntersection,
  Multiply,
} from "./common";

type Input = Split<Trim<typeof input>, "\n">;

type SizeX = `${Input["length"]}`;
type SizeY = `${Split<Input[0], "">["length"]}`;

type AllNodes = ParseInput<Input>;

type Numbers = FilterByType<AllNodes, "number">;
type Symbols = FilterByType<AllNodes, "symbol">;

type Part1 = SumValues<{
  [X in keyof AllNumberNeighbors]: SumValues<{
    [Y in keyof AllNumberNeighbors[X]]: AllNumberNeighbors[X][Y] extends []
      ? 0
      : GetNumber<X, Y>["value"];
  }>;
}>;

type Part2 = SumValues<{
  [X in keyof AllSymbolNeighbors]: SumValues<{
    [Y in keyof AllSymbolNeighbors[X]]: AllSymbolNeighbors[X][Y] extends [
      infer A extends SymbolNeighbor,
      infer B extends SymbolNeighbor,
    ]
      ? Multiply<A["value"], B["value"]>
      : "0";
  }>;
}>;

type Solution = [Part1, Part2];
//   ^?

type AllNumberNeighbors = {
  [X in keyof Numbers]: {
    [Y in keyof Numbers[X]]: Y extends string
      ? FindNumberNeighbors<X, Y>
      : never;
  };
};

type AllSymbolNeighbors = {
  [X in keyof Symbols]: {
    [Y in keyof Symbols[X]]: Y extends string
      ? FindSymbolNeighbors<X, Y>
      : never;
  };
};

type OccupiedByNumber = {
  [X in keyof Numbers]: UnionToIntersection<
    {
      [Y in keyof Numbers[X]]: Y extends string
        ? Numbers[X][Y] extends { value: infer V extends number }
          ? Range<Y, Sum<Y, NumberLength<V>>> extends infer R extends string[]
            ? { [Key in R[number]]: [X, Y] }
            : never
          : never
        : never;
    }[keyof Numbers[X]]
  >;
};

type SumValues<T> = SumValuesHelper<ObjectEntries<T>>;

type SumValuesHelper<T, Acc extends string = "0"> = [T] extends [never]
  ? Acc
  : LastInUnion<T> extends infer E
    ? E extends [unknown, infer V]
      ? SumValuesHelper<
          Exclude<T, E>,
          V extends `${number | bigint}` | number | bigint ? Sum<Acc, V> : Acc
        >
      : never
    : never;

type FilterByType<N extends Nodes, T extends Node["type"]> = {
  [Row in keyof N]: {
    [Col in keyof N[Row] as "type" extends keyof N[Row][Col]
      ? N[Row][Col]["type"] extends T
        ? Col
        : never
      : never]: N[Row][Col];
  };
};

type NumberNeighbor = SymbolNode & { x: string; y: string };

type FindNumberNeighbors<X extends string, Y extends string> = GetNumber<
  X,
  Y
> extends infer N extends NumberNode
  ? FindNumberNeighborsHelper<
      Max<Subtract<X, 1>, "0">,
      Max<Subtract<Y, 1>, "0">,
      Min<SizeX, Sum<X, 1>>,
      Min<SizeY, Sum<Y, NumberLength<N["value"]>>>
    >
  : never;

type FindNumberNeighborsHelper<
  X1 extends string,
  Y1 extends string,
  X2 extends string,
  Y2 extends string,
  X extends string = X1,
  Y extends string = Y1,
  Acc extends NumberNeighbor[] = [],
> = SmallerThanOrEqual<Y, Y2> extends false
  ? FindNumberNeighborsHelper<X1, Y1, X2, Y2, Sum<X, 1>, Y1, Acc>
  : SmallerThanOrEqual<X, X2> extends false
    ? Acc
    : GetSymbol<X, Y> extends infer S
      ? FindNumberNeighborsHelper<
          X1,
          Y1,
          X2,
          Y2,
          X,
          Sum<Y, 1>,
          [S] extends [never]
            ? Acc
            : S extends SymbolNode
              ? [...Acc, S & { x: X; y: Y }]
              : never
        >
      : never;

type FindSymbolNeighbors<
  X extends string,
  Y extends string,
> = FindSymbolNeighborsHelper<
  Max<Subtract<X, 1>, "0">,
  Max<Subtract<Y, 1>, "0">,
  Min<Sum<X, 1>, SizeX>,
  Min<Sum<Y, 1>, SizeY>
>;

type FindSymbolNeighborsHelper<
  X1 extends string,
  Y1 extends string,
  X2 extends string,
  Y2 extends string,
  X extends string = X1,
  Y extends string = Y1,
  Acc extends SymbolNeighbor[] = [],
  Seen extends [string, string] = never,
> = SmallerThanOrEqual<Y, Y2> extends false
  ? FindSymbolNeighborsHelper<X1, Y1, X2, Y2, Sum<X, 1>, Y1, Acc, Seen>
  : SmallerThanOrEqual<X, X2> extends false
    ? Acc
    : GetOccupyingNumber<X, Y> extends infer N
      ? [N] extends [never]
        ? FindSymbolNeighborsHelper<X1, Y1, X2, Y2, X, Sum<Y, 1>, Acc, Seen>
        : N extends [
              infer N extends NumberNode,
              infer NX extends string,
              infer NY extends string,
            ]
          ? FindSymbolNeighborsHelper<
              X1,
              Y1,
              X2,
              Y2,
              X,
              Sum<Y, 1>,
              [NX, NY] extends Seen ? Acc : [...Acc, N & { x: NX; y: NY }],
              Seen | [NX, NY]
            >
          : never
      : never;

type SymbolNeighbor = NumberNode & { x: string; y: string };

type GetNumber<X, Y> = X extends keyof Numbers
  ? Y extends keyof Numbers[X]
    ? Numbers[X][Y]
    : never
  : never;

type GetOccupyingNumber<X, Y> = X extends keyof OccupiedByNumber
  ? Y extends keyof OccupiedByNumber[X]
    ? OccupiedByNumber[X][Y] extends [infer NX, infer NY]
      ? GetNumber<NX, NY> extends infer N
        ? [N] extends [never]
          ? never
          : [N, NX, NY]
        : never
      : never
    : never
  : never;

type GetSymbol<X, Y> = X extends keyof Symbols
  ? Y extends keyof Symbols[X]
    ? Symbols[X][Y]
    : never
  : never;

type NumberLength<N extends number> = Split<`${N}`, "">["length"];

type Nodes = Record<string, Record<string, Node>>;

type NumberNode = {
  type: "number";
  value: number;
};

type SymbolNode = {
  type: "symbol";
  value: string;
};

type Node = NumberNode | SymbolNode;

type ParseInput<
  Rows extends string[],
  Index extends string = "0",
  Acc extends Nodes = {},
> = Rows extends [infer Head extends string, ...infer Rest extends string[]]
  ? ParseInput<Rest, Sum<Index, 1>, Acc & { [Key in Index]: ParseRow<Head> }>
  : Acc;

type ParseRow<
  Row extends string,
  Index extends string = "0",
  Acc extends Record<string, Node> = {},
> = Row extends `${infer Head}${infer Rest}`
  ? Head extends "."
    ? ParseRow<Rest, Sum<Index, 1>, Acc>
    : Head extends `${number}`
      ? TakeNumber<Row> extends [
          infer N extends number,
          infer Rest extends string,
        ]
        ? ParseRow<
            Rest,
            Sum<Index, NumberLength<N>>,
            Acc & { [Key in Index]: { type: "number"; value: N } }
          >
        : never
      : ParseRow<
          Rest,
          Sum<Index, 1>,
          Acc & { [Key in Index]: { type: "symbol"; value: Head } }
        >
  : Acc;

type TakeNumber<
  T extends string,
  Acc extends string = "",
> = T extends `${infer Head extends number}${infer Rest}`
  ? TakeNumber<Rest, `${Acc}${Head}`>
  : Acc extends `${infer V extends number}`
    ? [V, T]
    : never;

const input = `
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
`;
