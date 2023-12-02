import {
  Split,
  Trim,
  Id,
  Sum,
  SmallerThanOrEqual,
  Multiply,
  Max,
  All,
} from "./common";

type Input = Split<Trim<typeof input>, "\n">;

type Games = ParseGames<Input>;

type Solution = [Part1<Games>, Part2<Games>];
//   ^?

type Part1<T extends Game[], Acc extends string = "0"> = T extends [
  infer Head extends Game,
  ...infer Rest extends Game[],
]
  ? Part1<Rest, IsValid<Head> extends true ? Sum<Acc, Head["id"]> : Acc>
  : Acc;

type IsValid<G extends Game> = IsValidHelper<G["rounds"]>;

type IsValidHelper<R extends GameRound[]> = R extends [
  infer Head extends GameRound,
  ...infer Rest extends GameRound[],
]
  ? IsWithinLimits<Head> extends true
    ? IsValidHelper<Rest>
    : false
  : true;

type IsWithinLimits<R extends GameRound> = All<
  [
    SmallerThanOrEqual<R["red"], 12>,
    SmallerThanOrEqual<R["green"], 13>,
    SmallerThanOrEqual<R["blue"], 14>,
  ]
>;

type Part2<T extends Game[], Acc extends string = "0"> = T extends [
  infer Head extends Game,
  ...infer Rest extends Game[],
]
  ? Part2<Rest, Sum<Acc, Power<Head>>>
  : Acc;

type Power<G extends Game> = PowerHelper<G["rounds"]> extends infer M extends
  GameRound
  ? Multiply<Multiply<M["red"], M["green"]>, M["blue"]>
  : never;

type PowerHelper<
  R extends GameRound[],
  Acc extends GameRound = { red: 0; green: 0; blue: 0 },
> = R extends [infer Head extends GameRound, ...infer Rest extends GameRound[]]
  ? PowerHelper<
      Rest,
      {
        red: Max<Acc["red"], Head["red"]>;
        green: Max<Acc["green"], Head["green"]>;
        blue: Max<Acc["blue"], Head["blue"]>;
      }
    >
  : Acc;

type Game = {
  id: number;
  rounds: GameRound[];
};

type GameRound = {
  red: number;
  green: number;
  blue: number;
};

type ParseGames<
  Input extends string[],
  Acc extends Game[] = [],
> = Input extends [infer Head extends string, ...infer Rest extends string[]]
  ? ParseGames<Rest, [...Acc, ParseGame<Head>]>
  : Acc;

type ParseGame<Line extends string> = Line extends `Game ${infer Id extends
  number}: ${infer Rounds}`
  ? {
      id: Id;
      rounds: ParseRounds<Rounds>;
    }
  : never;

type ParseRounds<Rounds extends string> = Split<
  Rounds,
  ";"
> extends infer Parts extends string[]
  ? {
      [Key in keyof Parts]: ParseRound<Trim<Parts[Key]>>;
    }
  : never;

type ParseRound<Round extends string> = ParseRoundHelper<Split<Round, ",">>;

type ParseRoundHelper<
  Cubes extends string[],
  Acc extends GameRound = { red: 0; green: 0; blue: 0 },
> = Cubes extends [infer Head extends string, ...infer Rest extends string[]]
  ? Trim<Head> extends `${infer Amount extends number} ${infer Color extends
      keyof GameRound}`
    ? Omit<Acc, Color> & { [Key in Color]: Amount } extends infer NewAcc extends
        GameRound
      ? ParseRoundHelper<Rest, NewAcc>
      : never
    : never
  : Id<Acc>;

const input = `
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
`;
