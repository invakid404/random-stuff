type TicTacToeChip = "❌" | "⭕";
type TicTacToeEndState = "❌ Won" | "⭕ Won" | "Draw";
type TicTacToeState = TicTacToeChip | TicTacToeEndState;
type TicTacToeEmptyCell = "  ";
type TicTacToeCell = TicTacToeChip | TicTacToeEmptyCell;
type TicTacToeYPositions = "top" | "middle" | "bottom";
type TicTacToeXPositions = "left" | "center" | "right";
type TicTacToePositions = `${TicTacToeYPositions}-${TicTacToeXPositions}`;
type TicTactToeBoard = TicTacToeCell[][];
type TicTacToeGame = {
  board: TicTactToeBoard;
  state: TicTacToeState;
};

type EmptyBoard = [["  ", "  ", "  "], ["  ", "  ", "  "], ["  ", "  ", "  "]];

type NewGame = {
  board: EmptyBoard;
  state: "❌";
};

type YIndex = {
  top: 0;
  middle: 1;
  bottom: 2;
};

type XIndex = {
  left: 0;
  center: 1;
  right: 2;
};

type EvalState<
  Board extends TicTactToeBoard,
  State extends TicTacToeState,
> = State extends TicTacToeEndState
  ? State
  : State extends TicTacToeChip
    ? CheckWin<Board, State> extends true
      ? `${State} Won`
      : CheckDraw<Board> extends true
        ? "Draw"
        : Exclude<TicTacToeChip, State>
    : never;

type MyEqual<X, Y> = (<T>() => T extends X ? 1 : 2) extends <T>() => T extends Y
  ? 1
  : 2
  ? true
  : false;

type CheckWin<
  Board extends TicTactToeBoard,
  State extends TicTacToeChip,
> = CheckRows<Board, State> extends true
  ? true
  : CheckCols<Board, State> extends true
    ? true
    : false;

type CheckRows<
  Board extends TicTactToeBoard,
  State extends TicTacToeChip,
> = MyEqual<
  {
    [R in keyof Board]: [
      {
        [C in keyof Board[R]]: Board[R][C] extends State ? never : false;
      }[keyof Board[R] & `${number}`],
    ] extends [never]
      ? true
      : false;
  }[keyof Board & `${number}`],
  boolean
>;

type CheckCols<
  Board extends TicTactToeBoard,
  State extends TicTacToeChip,
> = MyEqual<
  Board[0] extends infer First extends TicTactToeBoard[number]
    ? {
        [C in keyof First]: [
          {
            [R in keyof Board]: C extends keyof Board[R]
              ? Board[R][C] extends State
                ? never
                : false
              : false;
          }[keyof Board & `${number}`],
        ] extends [never]
          ? true
          : false;
      }[keyof First & `${number}`]
    : never,
  boolean
>;

type CheckDraw<Board extends TicTactToeBoard> = MyEqual<
  {
    [R in keyof Board]: [
      {
        [C in keyof Board[R]]: Board[R][C] extends TicTacToeChip
          ? never
          : false;
      }[keyof Board[R] & `${number}`],
    ] extends [never]
      ? true
      : false;
  }[keyof Board & `${number}`],
  true
>;

type SetSquare<
  Board extends TicTactToeBoard,
  Value extends TicTacToeChip,
  Y extends keyof Board & number,
  X extends keyof Board[Y] & number,
  Acc extends TicTactToeBoard = [],
> = Board extends [
  infer Head extends TicTactToeBoard[number],
  ...infer Rest extends TicTactToeBoard,
]
  ? SetSquare<
      Rest,
      Value,
      Y,
      X,
      [...Acc, Acc["length"] extends Y ? SetInRow<Head, Value, X> : Head]
    >
  : Acc;

type SetInRow<
  Row extends TicTactToeBoard[number],
  Value extends TicTacToeChip,
  X extends keyof Row & number,
  Acc extends TicTactToeBoard[number] = [],
> = Row extends [
  infer Head extends TicTacToeCell,
  ...infer Rest extends TicTactToeBoard[number],
]
  ? SetInRow<Rest, Value, X, [...Acc, Acc["length"] extends X ? Value : Head]>
  : Acc;

type TicTacToe<
  Game extends TicTacToeGame,
  Move extends TicTacToePositions,
> = Game["state"] extends TicTacToeChip
  ? Move extends `${infer Y extends TicTacToeYPositions}-${infer X extends
      TicTacToeXPositions}`
    ? Game["board"][YIndex[Y]][XIndex[X]] extends "  "
      ? SetSquare<
          Game["board"],
          Game["state"],
          YIndex[Y],
          XIndex[X]
        > extends infer NewBoard extends TicTactToeBoard
        ? {
            board: NewBoard;
            state: EvalState<NewBoard, Game["state"]>;
          }
        : never
      : Game
    : never
  : Game;

import { Equal, Expect } from "type-testing";

type test_move1_actual = TicTacToe<NewGame, "top-center">;
//   ^?
type test_move1_expected = {
  board: [["  ", "❌", "  "], ["  ", "  ", "  "], ["  ", "  ", "  "]];
  state: "⭕";
};
type test_move1 = Expect<Equal<test_move1_actual, test_move1_expected>>;

type test_move2_actual = TicTacToe<test_move1_actual, "top-left">;
//   ^?
type test_move2_expected = {
  board: [["⭕", "❌", "  "], ["  ", "  ", "  "], ["  ", "  ", "  "]];
  state: "❌";
};
type test_move2 = Expect<Equal<test_move2_actual, test_move2_expected>>;

type test_move3_actual = TicTacToe<test_move2_actual, "middle-center">;
//   ^?
type test_move3_expected = {
  board: [["⭕", "❌", "  "], ["  ", "❌", "  "], ["  ", "  ", "  "]];
  state: "⭕";
};
type test_move3 = Expect<Equal<test_move3_actual, test_move3_expected>>;

type test_move4_actual = TicTacToe<test_move3_actual, "bottom-left">;
//   ^?
type test_move4_expected = {
  board: [["⭕", "❌", "  "], ["  ", "❌", "  "], ["⭕", "  ", "  "]];
  state: "❌";
};
type test_move4 = Expect<Equal<test_move4_actual, test_move4_expected>>;

type test_x_win_actual = TicTacToe<test_move4_actual, "bottom-center">;
//   ^?
type test_x_win_expected = {
  board: [["⭕", "❌", "  "], ["  ", "❌", "  "], ["⭕", "❌", "  "]];
  state: "❌ Won";
};
type test_x_win = Expect<Equal<test_x_win_actual, test_x_win_expected>>;

type type_move5_actual = TicTacToe<test_move4_actual, "bottom-right">;
//   ^?
type type_move5_expected = {
  board: [["⭕", "❌", "  "], ["  ", "❌", "  "], ["⭕", "  ", "❌"]];
  state: "⭕";
};
type test_move5 = Expect<Equal<type_move5_actual, type_move5_expected>>;

type test_o_win_actual = TicTacToe<type_move5_actual, "middle-left">;
//   ^?
type test_o_win_expected = {
  board: [["⭕", "❌", "  "], ["⭕", "❌", "  "], ["⭕", "  ", "❌"]];
  state: "⭕ Won";
};

// invalid move don't change the board and state
type test_invalid_actual = TicTacToe<test_move1_actual, "top-center">;
//   ^?
type test_invalid_expected = {
  board: [["  ", "❌", "  "], ["  ", "  ", "  "], ["  ", "  ", "  "]];
  state: "⭕";
};
type test_invalid = Expect<Equal<test_invalid_actual, test_invalid_expected>>;

type test_before_draw = {
  board: [["⭕", "❌", "⭕"], ["⭕", "❌", "❌"], ["❌", "⭕", "  "]];
  state: "⭕";
};
type test_draw_actual = TicTacToe<test_before_draw, "bottom-right">;
//   ^?
type test_draw_expected = {
  board: [["⭕", "❌", "⭕"], ["⭕", "❌", "❌"], ["❌", "⭕", "⭕"]];
  state: "Draw";
};
type test_draw = Expect<Equal<test_draw_actual, test_draw_expected>>;
