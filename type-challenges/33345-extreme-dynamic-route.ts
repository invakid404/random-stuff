/*
  33345 - Dynamic Route
  -------
  by 0753 (@0753Ljuc) #extreme

  ### Question

  Given below routes, infer its dynamic params.
  | Route                          | Params Type Definition                                                                                     |
  |--------------------------------|------------------------------------------------------------------------------------------------------------|
  | `/blog/[slug]/page.js`         | `{ slug: string }`                                                                                         |
  | `/shop/[...slug]/page.js`      | `{ slug: string[] }`                                                                                       |
  | `/shop/[[...slug]]/page.js`    | `{ slug?: string[] }`                                                                                      |
  | `/[categoryId]/[itemId]/page.js` | `{ categoryId: string, itemId: string }`                                                                 |
  | `/app/[...foo]/[...bar]`       | `never` - It's ambiguous as we cannot decide if `b` on `/app/a/b/c` is belongs to `foo` or `bar`.          |
  | `/[[...foo]]/[slug]/[...bar]`  | `never`                                                                                                    |
  | `/[first]/[[...foo]]/stub/[...bar]/[last]` | `{ first: string, foo?: string[], bar: string[], last: string }`                               |

  > View on GitHub: https://tsch.js.org/33345
*/

/* _____________ Your Code Here _____________ */
type Split<Value extends string, Delimiter extends string> = Value extends ''
  ? []
  : Value extends `${infer Prefix}${Delimiter}${infer Suffix}`
    ? [Prefix, ...Split<Suffix, Delimiter>]
    : [Value];

type TryParseManyOptional<T extends string> = T extends `[[...${infer Name}]]`
  ? Name extends ''
    ? false
    : Name
  : false;

type TryParseMany<T extends string> = T extends `[...${infer Name}]`
  ? Name extends ''
    ? false
    : Name
  : false;

type TryParseOne<T extends string> = T extends `[${infer Name}]`
  ? Name extends ''
    ? false
    : Name
  : false;

type ParsedPart = {
  type: 'plain' | 'one' | 'many' | 'manyOptional';
  key: string;
};

type ParsePart<T extends string> =
  TryParseManyOptional<T> extends infer Key extends string
    ? { type: 'manyOptional'; key: Key }
    : TryParseMany<T> extends infer Key extends string
      ? { type: 'many'; key: Key }
      : TryParseOne<T> extends infer Key extends string
        ? { type: 'one'; key: Key }
        : { type: 'plain'; key: T };

type ParseParts<Parts extends readonly string[]> = {
  [Key in keyof Parts]: ParsePart<Parts[Key]>;
};

type IsValid<
  SeenPlain extends boolean,
  IsMany extends boolean,
> = IsMany extends true ? (SeenPlain extends true ? true : false) : true;

type ValidateParts<
  Parts extends readonly ParsedPart[],
  SeenPlain extends boolean = true,
> = Parts extends [
  infer Head extends ParsedPart,
  ...infer Rest extends ParsedPart[],
]
  ? (
      Head['type'] extends `many${string}` ? true : false
    ) extends infer IsMany extends boolean
    ? IsValid<SeenPlain, IsMany> extends true
      ? ValidateParts<
          Rest,
          IsMany extends true
            ? false
            : Head['type'] extends 'plain'
              ? true
              : SeenPlain
        >
      : false
    : never
  : true;

type BuildPart<Part extends ParsedPart> = Part['type'] extends 'one'
  ? { [Key in Part['key']]: string }
  : Part['type'] extends 'many'
    ? { [Key in Part['key']]: string[] }
    : Part['type'] extends 'manyOptional'
      ? { [Key in Part['key']]?: string[] }
      : {};

type MergeParts<Parts extends readonly ParsedPart[], Acc = {}> = Parts extends [
  infer Head extends ParsedPart,
  ...infer Rest extends readonly ParsedPart[],
]
  ? MergeParts<Rest, Acc & BuildPart<Head>>
  : Omit<Acc, never>;

type DynamicRoute<T extends string> = ParseParts<
  Split<T, '/'>
> extends infer Parts extends readonly ParsedPart[]
  ? ValidateParts<Parts> extends true
    ? MergeParts<Parts>
    : never
  : never;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<DynamicRoute<'/shop'>, {}>>,
  Expect<Equal<DynamicRoute<'/shop/[]'>, {}>>,
  Expect<Equal<DynamicRoute<'/shop/[slug]'>, { slug: string }>>,
  Expect<Equal<DynamicRoute<'/shop/[slug]/'>, { slug: string }>>,
  Expect<
    Equal<DynamicRoute<'/shop/[slug]/[foo]'>, { slug: string; foo: string }>
  >,
  Expect<
    Equal<
      DynamicRoute<'/shop/[slug]/stub/[foo]'>,
      { slug: string; foo: string }
    >
  >,
  Expect<
    Equal<
      DynamicRoute<'/shop/[slug]/stub/[foo]'>,
      { slug: string; foo: string }
    >
  >,
  Expect<
    Equal<
      DynamicRoute<'/shop/[slug]/stub/[...foo]'>,
      { slug: string; foo: string[] }
    >
  >,
  Expect<
    Equal<
      DynamicRoute<'/shop/[slug]/stub/[[...foo]]'>,
      { slug: string; foo?: string[] }
    >
  >,
  Expect<
    Equal<
      DynamicRoute<'/shop/[slug]/stub/[[...foo]]/[]'>,
      { slug: string; foo?: string[] }
    >
  >,
  Expect<
    Equal<
      DynamicRoute<'/shop/[slug]/stub/[[...foo]]/[...]'>,
      { slug: string; foo?: string[]; '...': string }
    >
  >,
  Expect<
    Equal<
      DynamicRoute<'/shop/[slug]/stub/[[...foo]]/[...]/[]index.html'>,
      { slug: string; foo?: string[]; '...': string }
    >
  >,
  Expect<
    Equal<
      DynamicRoute<'/shop/[slug]/stub/[[...foo]]/[...]/[...]index.html'>,
      { slug: string; foo?: string[]; '...': string }
    >
  >,
  Expect<Equal<DynamicRoute<'/[slug]/[[...foo]]/[...bar]'>, never>>,
  Expect<Equal<DynamicRoute<'/[[...foo]]/[slug]/[...bar]'>, never>>,
  Expect<Equal<DynamicRoute<'/[[...foo]]/[...bar]/static'>, never>>,
  Expect<
    Equal<
      DynamicRoute<'[[...foo]]/stub/[...bar]'>,
      { foo?: string[]; bar: string[] }
    >
  >,
  Expect<
    Equal<
      DynamicRoute<'[first]/[[...foo]]/stub/[...bar]/[last]'>,
      { first: string; foo?: string[]; bar: string[]; last: string }
    >
  >,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/33345/answer
  > View solutions: https://tsch.js.org/33345/solutions
  > More Challenges: https://tsch.js.org
*/
