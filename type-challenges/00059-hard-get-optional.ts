/*
  59 - Get Optional
  -------
  by Zheeeng (@zheeeng) #hard #utils #infer
  
  ### Question
  
  Implement the advanced util type `GetOptional<T>`, which remains all the optional fields
  
  For example
  
  ```ts
  type I = GetOptional<{ foo: number, bar?: string }> // expected to be { bar?: string }
  ```
  
  > View on GitHub: https://tsch.js.org/59
*/

/* _____________ Your Code Here _____________ */
type MyEqual<X, Y> = (<T>() => T extends X ? 1 : 2) extends <T>() => T extends Y
  ? 1
  : 2
  ? true
  : false;

type GetOptionalKeys<T> = {
  [key in keyof Required<T>]: MyEqual<
    Pick<T, key>,
    Partial<Pick<T, key>>
  > extends true
    ? key
    : never;
}[keyof T];

type GetOptional<T> = Pick<T, GetOptionalKeys<T>>;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<Equal<GetOptional<{ foo: number; bar?: string }>, { bar?: string }>>,
  Expect<
    Equal<GetOptional<{ foo: undefined; bar?: undefined }>, { bar?: undefined }>
  >,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/59/answer
  > View solutions: https://tsch.js.org/59/solutions
  > More Challenges: https://tsch.js.org
*/
