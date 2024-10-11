/*
  33616 - Extract Tag Key From Tagged Union
  -------
  by Juyeong Maing (@mjy9088) #hard

  ### Question

  A tagged union, or discriminated union, is a union type in which every member type is distinguishable by a property called the tag.

  Write an `ExtractTagFromTaggedUnion` type that takes a tagged union and yields the name of the tag property.

  * The tag must be a string.

  ```ts
  type MyTaggedUnion = { type: 'position', position: [x: number, y: number]} | { type: 'direction', direction: [x: number, y: number]}
  type Tag = ExtractTagFromTaggedUnion<MyTaggedUnion>; // 'type' expected.
  ```

  > View on GitHub: https://tsch.js.org/33616
*/

/* _____________ Your Code Here _____________ */
type UnionToIntersection<U> = (
  U extends any ? (arg: U) => any : never
) extends (arg: infer I) => void
  ? I
  : never;

type LastInUnion<U> = UnionToIntersection<
  U extends any ? (arg: U) => void : never
> extends (arg: infer Last) => void
  ? Last
  : never;

type UnionToTuple<U> = [U] extends [never]
  ? []
  : LastInUnion<U> extends infer Last
    ? [...UnionToTuple<Exclude<U, Last>>, Last]
    : never;

type StringLiteralKeys<
  T extends readonly object[],
  Acc extends readonly string[] = [],
> = T extends [infer Head, ...infer Rest extends readonly object[]]
  ? StringLiteralKeys<
      Rest,
      [
        ...Acc,
        keyof {
          [Key in keyof Head as Head[Key] extends string
            ? string extends Head[Key]
              ? never
              : Key
            : never]: never;
        } &
          string,
      ]
    >
  : Acc;

type IntersectAll<
  T extends readonly string[],
  Acc extends string = string,
> = T extends [
  infer Head extends string,
  ...infer Rest extends readonly string[],
]
  ? IntersectAll<Rest, Acc & Head>
  : Acc;

type GetCandidates<V extends readonly object[]> =
  StringLiteralKeys<V> extends infer Keys extends readonly string[]
    ? IntersectAll<Keys>
    : never;

type CheckCandidate<
  V extends readonly object[],
  Candidate extends string,
  Seen extends string = never,
> = V extends [infer Head, ...infer Rest extends readonly object[]]
  ? Candidate extends keyof Head
    ? Head[Candidate] extends infer Value extends string
      ? Value extends Seen
        ? false
        : CheckCandidate<Rest, Candidate, Seen | Value>
      : never
    : never
  : true;

type FilterCandidates<
  V extends readonly object[],
  Candidates extends string,
> = Candidates extends unknown
  ? CheckCandidate<V, Candidates> extends true
    ? Candidates
    : never
  : never;

type ExtractTagFromTaggedUnion<T> = UnionToTuple<T> extends infer V extends
  readonly object[]
  ? GetCandidates<V> extends infer Candidates extends string
    ? FilterCandidates<V, Candidates>
    : never
  : never;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type Foo = { type: 'foo'; foo: 'hello' } | { type: 'bar'; bar: 'world' };
type Bar = { foo: 'foo' } | { bar: 'bar' };
type Baz =
  | { type: 'number'; value: number }
  | { type: 'string'; value: string };
type Qux =
  | { type: 'position'; position: [x: number, y: number] }
  | { type: 'direction'; direction: [x: number, y: number] };
type Quux =
  | { type: 'position'; value: [x: number, y: number] }
  | { type: 'direction'; value: [x: number, y: number] };
type Corge = { seq: '1'; type: 'one' } | { seq: '2'; type: 'two' };
type Grault =
  | { type: 'A'; value: number }
  | { type: 'B'; value: number }
  | { type: 'B'; value: string };
type Garply = { type: 'only' };
type Waldo = { type: string };
type Fred = { type: 1 } | { type: '2' };
type Plugh = {};
type Xyzzy = { type: 'same'; same: number } | { type: 'same'; same: number };
type Thud =
  | { type: 'same'; same: number }
  | { type: 'same'; same: number; really: false };

type cases = [
  Expect<Equal<ExtractTagFromTaggedUnion<Foo>, 'type'>>,
  Expect<Equal<ExtractTagFromTaggedUnion<Bar>, never>>,
  Expect<Equal<ExtractTagFromTaggedUnion<Baz>, 'type'>>,
  Expect<Equal<ExtractTagFromTaggedUnion<Qux>, 'type'>>,
  Expect<Equal<ExtractTagFromTaggedUnion<Quux>, 'type'>>,
  Expect<Equal<ExtractTagFromTaggedUnion<Corge>, 'seq' | 'type'>>,
  Expect<Equal<ExtractTagFromTaggedUnion<Grault>, never>>,
  Expect<Equal<ExtractTagFromTaggedUnion<Garply>, 'type'>>,
  Expect<Equal<ExtractTagFromTaggedUnion<Waldo>, never>>,
  Expect<Equal<ExtractTagFromTaggedUnion<Fred>, never>>,
  Expect<Equal<ExtractTagFromTaggedUnion<Plugh>, never>>,
  Expect<Equal<ExtractTagFromTaggedUnion<Xyzzy>, 'type'>>,
  Expect<Equal<ExtractTagFromTaggedUnion<Thud>, never>>,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/33616/answer
  > View solutions: https://tsch.js.org/33616/solutions
  > More Challenges: https://tsch.js.org
*/
