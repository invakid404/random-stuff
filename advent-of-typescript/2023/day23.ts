type Connect4Chips = "🔴" | "🟡";
type Connect4Cell = Connect4Chips | "  ";
type Connect4State = "🔴" | "🟡" | "🔴 Won" | "🟡 Won" | "Draw";

type EmptyBoard = [
  ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
  ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
  ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
  ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
  ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
  ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
];

type NewGame = {
  board: EmptyBoard;
  state: "🟡";
};

type Connect4<
  G extends Game,
  Col extends number,
> = G["state"] extends Connect4Chips
  ? PlayMove<G["board"], G["state"], Col> extends {
      board: infer B extends Board;
      row: infer Row extends number;
      col: infer Col extends number;
    }
    ? Row extends -1
      ? G
      : {
          board: B;
          state: EvalState<B, G["state"], Row, Col>;
        }
    : never
  : G;

type Board = Connect4Cell[][];

type Game = {
  board: Board;
  state: Connect4State;
};

type PlayMove<
  B extends Board,
  S extends Connect4Chips,
  Col extends number,
  RowIdx extends readonly unknown[] = Tail<NumberToTuple<B["length"]>>,
  Acc extends Board = [],
> = B extends [...infer Init extends Board, infer Last extends Board[number]]
  ? Col extends keyof Last
    ? Last[Col] extends "  "
      ? {
          board: [
            ...Init,
            {
              [Key in keyof Last]: Key extends `${Col}` ? S : Last[Key];
            },
            ...Acc,
          ];
          row: RowIdx["length"];
          col: Col;
        }
      : PlayMove<Init, S, Col, Tail<RowIdx>, [Last, ...Acc]>
    : never
  : { board: Acc; row: -1; col: Col };

type Tail<T extends readonly unknown[]> = T extends [unknown, ...infer Tail]
  ? Tail
  : never;

type EvalState<
  B extends Board,
  S extends Connect4Chips,
  Row extends number,
  Col extends number,
> = CheckWin<B, S, Row, Col> extends true
  ? `${S} Won`
  : IsDraw<B> extends true
    ? "Draw"
    : Exclude<Connect4Chips, S>;

type Directions = [
  [0, -1, 0, 1], // Row
  [-1, 0, 1, 0], // Col
  [-1, -1, 1, 1], // Diagonal A
  [-1, 1, 1, -1], // Diagonal B
];

type Direction = Directions[number];

type CheckWin<
  B extends Board,
  S extends Connect4Chips,
  Row extends number,
  Col extends number,
  D extends Direction[] = Directions,
> = D extends [infer Head extends Direction, ...infer Rest extends Direction[]]
  ? CountDirection<B, S, Row, Col, Head> extends 4
    ? true
    : CheckWin<B, S, Row, Col, Rest>
  : false;

type CountDirection<
  B extends Board,
  S extends Connect4Chips,
  Row extends number,
  Col extends number,
  D extends Direction,
> = D extends [
  infer DRow1 extends Delta,
  infer DCol1 extends Delta,
  infer DRow2 extends Delta,
  infer DCol2 extends Delta,
]
  ? CountWhile<B, S, Row, Col, DRow1, DCol1> extends infer Left extends
      readonly unknown[]
    ? CountWhile<B, S, Row, Col, DRow2, DCol2> extends infer Right extends
        readonly unknown[]
      ? [...Tail<Left>, ...Tail<Right>, unknown] extends infer Total extends
          readonly unknown[]
        ? Total["length"]
        : never
      : never
    : never
  : never;

type Delta = -1 | 0 | 1;

type ApplyDelta<N extends readonly unknown[], D extends Delta> = D extends -1
  ? N extends [unknown, ...infer Rest]
    ? Rest
    : never
  : D extends 0
    ? N
    : [...N, unknown];

type CountWhile<
  B extends Board,
  S extends Connect4Chips,
  Row extends number,
  Col extends number,
  DRow extends Delta,
  DCol extends Delta,
> = CountWhileHelper<B, S, NumberToTuple<Row>, NumberToTuple<Col>, DRow, DCol>;

type CountWhileHelper<
  B extends Board,
  S extends Connect4Chips,
  Row extends readonly unknown[],
  Col extends readonly unknown[],
  DRow extends Delta,
  DCol extends Delta,
  Acc extends readonly unknown[] = [],
> = Acc["length"] extends 4
  ? Acc
  : [Row] extends [never]
    ? Acc
    : [Col] extends [never]
      ? Acc
      : Row["length"] extends keyof B
        ? Col["length"] extends keyof B[Row["length"]]
          ? B[Row["length"]][Col["length"]] extends S
            ? CountWhileHelper<
                B,
                S,
                ApplyDelta<Row, DRow>,
                ApplyDelta<Col, DCol>,
                DRow,
                DCol,
                [...Acc, unknown]
              >
            : Acc
          : Acc
        : Acc;

type IsDraw<B extends Board> = "  " extends B[number][number] ? false : true;

type NumberToTuple<
  N extends number,
  Acc extends readonly unknown[] = [],
> = Acc["length"] extends N ? Acc : NumberToTuple<N, [...Acc, unknown]>;

import { Expect, Equal } from "type-testing";

type test_move1_actual = Connect4<NewGame, 0>;
//   ^?
type test_move1_expected = {
  board: [
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["🟡", "  ", "  ", "  ", "  ", "  ", "  "],
  ];
  state: "🔴";
};
type test_move1 = Expect<Equal<test_move1_actual, test_move1_expected>>;

type test_move2_actual = Connect4<test_move1_actual, 0>;
//   ^?
type test_move2_expected = {
  board: [
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["🔴", "  ", "  ", "  ", "  ", "  ", "  "],
    ["🟡", "  ", "  ", "  ", "  ", "  ", "  "],
  ];
  state: "🟡";
};
type test_move2 = Expect<Equal<test_move2_actual, test_move2_expected>>;

type test_move3_actual = Connect4<test_move2_actual, 0>;
//   ^?
type test_move3_expected = {
  board: [
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["🟡", "  ", "  ", "  ", "  ", "  ", "  "],
    ["🔴", "  ", "  ", "  ", "  ", "  ", "  "],
    ["🟡", "  ", "  ", "  ", "  ", "  ", "  "],
  ];
  state: "🔴";
};
type test_move3 = Expect<Equal<test_move3_actual, test_move3_expected>>;

type test_move4_actual = Connect4<test_move3_actual, 1>;
//   ^?
type test_move4_expected = {
  board: [
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["🟡", "  ", "  ", "  ", "  ", "  ", "  "],
    ["🔴", "  ", "  ", "  ", "  ", "  ", "  "],
    ["🟡", "🔴", "  ", "  ", "  ", "  ", "  "],
  ];
  state: "🟡";
};
type test_move4 = Expect<Equal<test_move4_actual, test_move4_expected>>;

type test_move5_actual = Connect4<test_move4_actual, 2>;
//   ^?
type test_move5_expected = {
  board: [
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["🟡", "  ", "  ", "  ", "  ", "  ", "  "],
    ["🔴", "  ", "  ", "  ", "  ", "  ", "  "],
    ["🟡", "🔴", "🟡", "  ", "  ", "  ", "  "],
  ];
  state: "🔴";
};
type test_move5 = Expect<Equal<test_move5_actual, test_move5_expected>>;

type test_move6_actual = Connect4<test_move5_actual, 1>;
//   ^?
type test_move6_expected = {
  board: [
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["🟡", "  ", "  ", "  ", "  ", "  ", "  "],
    ["🔴", "🔴", "  ", "  ", "  ", "  ", "  "],
    ["🟡", "🔴", "🟡", "  ", "  ", "  ", "  "],
  ];
  state: "🟡";
};
type test_move6 = Expect<Equal<test_move6_actual, test_move6_expected>>;

type test_red_win_actual = Connect4<
  {
    board: [
      ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
      ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
      ["🟡", "  ", "  ", "  ", "  ", "  ", "  "],
      ["🟡", "  ", "  ", "  ", "  ", "  ", "  "],
      ["🔴", "🔴", "🔴", "  ", "  ", "  ", "  "],
      ["🟡", "🔴", "🟡", "🟡", "  ", "  ", "  "],
    ];
    state: "🔴";
  },
  3
>;

type test_red_win_expected = {
  board: [
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["🟡", "  ", "  ", "  ", "  ", "  ", "  "],
    ["🟡", "  ", "  ", "  ", "  ", "  ", "  "],
    ["🔴", "🔴", "🔴", "🔴", "  ", "  ", "  "],
    ["🟡", "🔴", "🟡", "🟡", "  ", "  ", "  "],
  ];
  state: "🔴 Won";
};

type test_red_win = Expect<Equal<test_red_win_actual, test_red_win_expected>>;

type test_yellow_win_actual = Connect4<
  {
    board: [
      ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
      ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
      ["🔴", "  ", "  ", "  ", "  ", "  ", "  "],
      ["🟡", "  ", "  ", "  ", "  ", "  ", "  "],
      ["🔴", "  ", "🔴", "🔴", "  ", "  ", "  "],
      ["🟡", "  ", "🟡", "🟡", "  ", "  ", "  "],
    ];
    state: "🟡";
  },
  1
>;

type test_yellow_win_expected = {
  board: [
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["🔴", "  ", "  ", "  ", "  ", "  ", "  "],
    ["🟡", "  ", "  ", "  ", "  ", "  ", "  "],
    ["🔴", "  ", "🔴", "🔴", "  ", "  ", "  "],
    ["🟡", "🟡", "🟡", "🟡", "  ", "  ", "  "],
  ];
  state: "🟡 Won";
};

type test_yellow_win = Expect<
  Equal<test_yellow_win_actual, test_yellow_win_expected>
>;

type test_diagonal_yellow_win_actual = Connect4<
  {
    board: [
      ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
      ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
      ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
      ["  ", "  ", "🟡", "🔴", "  ", "  ", "  "],
      ["🔴", "🟡", "🔴", "🔴", "  ", "  ", "  "],
      ["🟡", "🔴", "🟡", "🟡", "  ", "  ", "  "],
    ];
    state: "🟡";
  },
  3
>;

type test_diagonal_yellow_win_expected = {
  board: [
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["  ", "  ", "  ", "🟡", "  ", "  ", "  "],
    ["  ", "  ", "🟡", "🔴", "  ", "  ", "  "],
    ["🔴", "🟡", "🔴", "🔴", "  ", "  ", "  "],
    ["🟡", "🔴", "🟡", "🟡", "  ", "  ", "  "],
  ];
  state: "🟡 Won";
};

type test_diagonal_yellow_win = Expect<
  Equal<test_diagonal_yellow_win_actual, test_diagonal_yellow_win_expected>
>;

type test_draw_actual = Connect4<
  {
    board: [
      ["🟡", "🔴", "🔴", "🟡", "🟡", "🔴", "  "],
      ["🔴", "🟡", "🟡", "🔴", "🔴", "🟡", "🔴"],
      ["🟡", "🔴", "🔴", "🟡", "🟡", "🔴", "🟡"],
      ["🔴", "🟡", "🟡", "🔴", "🔴", "🟡", "🔴"],
      ["🟡", "🔴", "🔴", "🟡", "🟡", "🔴", "🟡"],
      ["🔴", "🟡", "🟡", "🔴", "🔴", "🟡", "🔴"],
    ];
    state: "🟡";
  },
  6
>;

type test_draw_expected = {
  board: [
    ["🟡", "🔴", "🔴", "🟡", "🟡", "🔴", "🟡"],
    ["🔴", "🟡", "🟡", "🔴", "🔴", "🟡", "🔴"],
    ["🟡", "🔴", "🔴", "🟡", "🟡", "🔴", "🟡"],
    ["🔴", "🟡", "🟡", "🔴", "🔴", "🟡", "🔴"],
    ["🟡", "🔴", "🔴", "🟡", "🟡", "🔴", "🟡"],
    ["🔴", "🟡", "🟡", "🔴", "🔴", "🟡", "🔴"],
  ];
  state: "Draw";
};

type test_draw = Expect<Equal<test_draw_actual, test_draw_expected>>;
