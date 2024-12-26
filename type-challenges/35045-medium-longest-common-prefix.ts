/*
  35045 - Longest Common Prefix
  -------
  by Tom Cleary (@thomcleary) #medium

  ### Question

  ### Longest Common Prefix

  Write a type, `LongestCommonPrefix` that returns the longest common prefix string amongst a tuple of strings.

  If there is no common prefix, return an empty string `""`.

  ```ts
  type Common = LongestCommonPrefix<["flower", "flow", "flight"]>
  //   ?^ "fl"

  type Uncommon = LongestCommonPrefix<["dog", "racecar", "race"]>
  //   ?^ ""
  ```
  Inspired by [LeetCode 14. Longest Common Prefix](https://leetcode.com/problems/longest-common-prefix/description/)

  > View on GitHub: https://tsch.js.org/35045
*/

/* _____________ Your Code Here _____________ */
type LongestCommonPrefix<T extends string[]> = '' extends T[number]
  ? ''
  : MakeSameLength<T> extends infer T extends string[]
    ? LongestCommonSuffix<{
        [Key in keyof T]: Reverse<T[Key]>;
      }> extends infer R extends string
      ? Reverse<R>
      : never
    : never;

type LongestCommonSuffix<T extends string[]> =
  IsUnion<T[number]> extends false
    ? T[number]
    : LongestCommonSuffix<{
        [Key in keyof T]: T[Key] extends `${string}${infer Rest}`
          ? Rest
          : never;
      }>;

type Reverse<
  S extends string,
  Acc extends string = '',
> = S extends `${infer Head}${infer Rest}`
  ? Reverse<Rest, `${Head}${Acc}`>
  : Acc;

type MakeSameLength<
  T extends string[],
  Acc extends string[] = [],
> = '' extends T[number]
  ? Acc
  : MakeSameLength<
      {
        [Key in keyof T]: T[Key] extends `${string}${infer Rest}`
          ? Rest
          : never;
      },
      {
        [Key in keyof T]: T[Key] extends `${infer Head}${string}`
          ? Key extends keyof Acc
            ? `${Acc[Key]}${Head}`
            : Head
          : never;
      }
    >;

type IsUnion<T, U = T> = [T] extends [never]
  ? false
  : T extends any
    ? [U] extends [T]
      ? false
      : true
    : never;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<LongestCommonPrefix<['flower', 'flow', 'flight']>, 'fl'>>,
  Expect<Equal<LongestCommonPrefix<['dog', 'racecar', 'race']>, ''>>,
  Expect<Equal<LongestCommonPrefix<['', '', '']>, ''>>,
  Expect<Equal<LongestCommonPrefix<['a', '', '']>, ''>>,
  Expect<Equal<LongestCommonPrefix<['', 'a', '']>, ''>>,
  Expect<Equal<LongestCommonPrefix<['', '', 'a']>, ''>>,
  Expect<Equal<LongestCommonPrefix<['a', 'a', '']>, ''>>,
  Expect<Equal<LongestCommonPrefix<['a', '', 'a']>, ''>>,
  Expect<Equal<LongestCommonPrefix<['', 'a', 'a']>, ''>>,
  Expect<Equal<LongestCommonPrefix<['a', 'a', 'a']>, 'a'>>,
  Expect<Equal<LongestCommonPrefix<['abc', 'abcd', 'abcde']>, 'abc'>>,
  Expect<Equal<LongestCommonPrefix<[' ', ' ', ' ']>, ' '>>,
  Expect<
    Equal<
      LongestCommonPrefix<['type-challenges', 'type-hero', 'typescript']>,
      'type'
    >
  >,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/35045/answer
  > View solutions: https://tsch.js.org/35045/solutions
  > More Challenges: https://tsch.js.org
*/
