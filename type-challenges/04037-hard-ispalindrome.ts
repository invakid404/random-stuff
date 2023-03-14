/*
  4037 - IsPalindrome
  -------
  by jiangshan (@jiangshanmeta) #hard #string

  ### Question

  Implement type ```IsPalindrome<T>``` to check whether  a string or number is palindrome.

  For example:

  ```typescript
  IsPalindrome<'abc'> // false
  IsPalindrome<121> // true
  ```

  > View on GitHub: https://tsch.js.org/4037
*/

/* _____________ Your Code Here _____________ */
type Chars<
  T extends string,
  Acc extends readonly string[] = [],
> = T extends `${infer Head}${infer Rest}` ? Chars<Rest, [...Acc, Head]> : Acc;

type IsPalindromeRec<T extends readonly string[]> = T['length'] extends 0
  ? true
  : T['length'] extends 1
  ? true
  : T extends [infer First, ...infer Rest]
  ? Rest extends [...infer Init extends readonly string[], infer Last]
    ? First extends Last
      ? IsPalindromeRec<Init>
      : false
    : never
  : never;

type IsPalindrome<T extends string | number> = IsPalindromeRec<Chars<`${T}`>>;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<IsPalindrome<'abc'>, false>>,
  Expect<Equal<IsPalindrome<'b'>, true>>,
  Expect<Equal<IsPalindrome<'abca'>, false>>,
  Expect<Equal<IsPalindrome<'abcba'>, true>>,
  Expect<Equal<IsPalindrome<121>, true>>,
  Expect<Equal<IsPalindrome<19260817>, false>>,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/4037/answer
  > View solutions: https://tsch.js.org/4037/solutions
  > More Challenges: https://tsch.js.org
*/
