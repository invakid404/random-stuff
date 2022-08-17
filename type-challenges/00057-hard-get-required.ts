/*
  57 - Get Required
  -------
  by Zheeeng (@zheeeng) #hard #utils #infer
  
  ### Question
  
  Implement the advanced util type `GetRequired<T>`, which remains all the required fields
  
  For example
  
  ```ts
  type I = GetRequired<{ foo: number, bar?: string }> // expected to be { foo: number }
  ```
  
  > View on GitHub: https://tsch.js.org/57
*/

/* _____________ Your Code Here _____________ */
type MyEqual<X, Y> = (<T>() => T extends X ? 1 : 2) extends <T>() => T extends Y
  ? 1
  : 2
  ? true
  : false;

type GetRequiredKeys<T> = {
  [key in keyof T]-?: MyEqual<Pick<T, key>, Required<Pick<T, key>>> extends true
    ? key
    : never;
}[keyof T];

type GetRequired<T> = Pick<T, GetRequiredKeys<T>>;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<GetRequired<{ foo: number; bar?: string }>, { foo: number }>>,
  Expect<
    Equal<GetRequired<{ foo: undefined; bar?: undefined }>, { foo: undefined }>
  >,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/57/answer
  > View solutions: https://tsch.js.org/57/solutions
  > More Challenges: https://tsch.js.org
*/
