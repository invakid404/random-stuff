type Letters = {
  A: ["█▀█ ", "█▀█ ", "▀ ▀ "];
  B: ["█▀▄ ", "█▀▄ ", "▀▀  "];
  C: ["█▀▀ ", "█ ░░", "▀▀▀ "];
  E: ["█▀▀ ", "█▀▀ ", "▀▀▀ "];
  H: ["█ █ ", "█▀█ ", "▀ ▀ "];
  I: ["█ ", "█ ", "▀ "];
  M: ["█▄░▄█ ", "█ ▀ █ ", "▀ ░░▀ "];
  N: ["█▄░█ ", "█ ▀█ ", "▀ ░▀ "];
  P: ["█▀█ ", "█▀▀ ", "▀ ░░"];
  R: ["█▀█ ", "██▀ ", "▀ ▀ "];
  S: ["█▀▀ ", "▀▀█ ", "▀▀▀ "];
  T: ["▀█▀ ", "░█ ░", "░▀ ░"];
  Y: ["█ █ ", "▀█▀ ", "░▀ ░"];
  W: ["█ ░░█ ", "█▄▀▄█ ", "▀ ░ ▀ "];
  " ": ["░", "░", "░"];
  ":": ["#", "░", "#"];
  "*": ["░", "#", "░"];
};

type Line = [string, string, string];

type EmptyLine = ["", "", ""];

type AddCharacter<
  L extends Line,
  C extends keyof Letters,
> = Letters[C] extends infer R extends Line
  ? [`${L[0]}${R[0]}`, `${L[1]}${R[1]}`, `${L[2]}${R[2]}`]
  : never;

type ToAsciiArt<T extends string> = ToAsciiArtHelper<Uppercase<T>>;

type ToAsciiArtHelper<
  T extends string,
  L extends Line = EmptyLine,
  Acc extends string[] = [],
> = T extends `\n${infer Rest}`
  ? ToAsciiArtHelper<Rest, EmptyLine, [...Acc, ...L]>
  : T extends `${infer Head extends keyof Letters}${infer Rest}`
    ? ToAsciiArtHelper<Rest, AddCharacter<L, Head>, Acc>
    : L extends EmptyLine
      ? Acc
      : [...Acc, ...L];
