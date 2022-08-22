/*
  216 - Slice
  -------
  by Anthony Fu (@antfu) #extreme #array
  
  ### Question
  
  Implement the JavaScript `Array.slice` function in the type system. `Slice<Arr, Start, End>` takes the three argument. The output should be a subarray of `Arr` from index `Start` to `End`. Indexes with negative numbers should be counted from reversely.
  
  For example
  
  ```ts
  type Arr = [1, 2, 3, 4, 5]
  type Result = Slice<Arr, 2, 4> // expected to be [3, 4]
  ```
  
  > View on GitHub: https://tsch.js.org/216
*/

/* _____________ Your Code Here _____________ */

type Init<Arr extends readonly any[]> = Arr extends [...infer Init, any]
  ? Init
  : [];

type Tail<Arr extends readonly any[]> = Arr extends [any, ...infer Tail]
  ? Tail
  : [];

type NegativeToPositive<
  Arr extends readonly any[],
  Amount extends number
> = `${Amount}` extends `-${number}`
  ? NegativeToPositiveRec<Arr, Amount>
  : Amount;

type NegativeToPositiveRec<
  Arr extends readonly any[],
  Amount extends number,
  Accumulator extends readonly any[] = []
> = `${Amount}` extends `-${Accumulator["length"]}`
  ? Arr["length"]
  : NegativeToPositiveRec<Init<Arr>, Amount, [...Accumulator, any]>;

type Split<
  Arr extends readonly any[],
  Index extends number,
  InitAccumulator extends readonly any[] = []
> = Arr extends []
  ? [InitAccumulator, Arr]
  : InitAccumulator["length"] extends Index
  ? [InitAccumulator, Arr]
  : Split<Tail<Arr>, Index, [...InitAccumulator, Arr[0]]>;

type Slice<
  Arr extends readonly any[],
  Start extends number = 0,
  End extends number = Arr["length"]
> = Split<Arr, NegativeToPositive<Arr, End>> extends [
  infer Init extends readonly any[],
  any
]
  ? Split<Init, NegativeToPositive<Arr, Start>> extends [any, infer Result]
    ? Result
    : []
  : [];

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from "@type-challenges/utils";

type Arr = [1, 2, 3, 4, 5];

type cases = [
  // basic
  Expect<Equal<Slice<Arr, 0, 1>, [1]>>,
  Expect<Equal<Slice<Arr, 0, 0>, []>>,
  Expect<Equal<Slice<Arr, 2, 4>, [3, 4]>>,

  // optional args
  Expect<Equal<Slice<[]>, []>>,
  Expect<Equal<Slice<Arr>, Arr>>,
  Expect<Equal<Slice<Arr, 0>, Arr>>,
  Expect<Equal<Slice<Arr, 2>, [3, 4, 5]>>,

  // negative index
  Expect<Equal<Slice<Arr, 0, -1>, [1, 2, 3, 4]>>,
  Expect<Equal<Slice<Arr, -3, -1>, [3, 4]>>,

  // invalid
  Expect<Equal<Slice<Arr, 10>, []>>,
  Expect<Equal<Slice<Arr, 1, 0>, []>>,
  Expect<Equal<Slice<Arr, 10, 20>, []>>
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/216/answer
  > View solutions: https://tsch.js.org/216/solutions
  > More Challenges: https://tsch.js.org
*/
