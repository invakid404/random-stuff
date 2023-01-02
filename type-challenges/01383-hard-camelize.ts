/*
  1383 - Camelize
  -------
  by Denis (@denchiklut) #hard #union #recursion

  ### Question

  Implement Camelize which converts object from snake_case to to camelCase

  ```ts
  Camelize<{
    some_prop: string,
    prop: { another_prop: string },
    array: [{ snake_case: string }]
  }>

  // expected to be
  // {
  //   someProp: string,
  //   prop: { anotherProp: string },
  //   array: [{ snakeCase: string }]
  // }
  ```

  > View on GitHub: https://tsch.js.org/1383
*/

/* _____________ Your Code Here _____________ */
type CapitalizeFirst<Value extends string> = Capitalize<Lowercase<Value>>;

type UpperCamelCase<Value extends string> =
  Value extends `${infer First}_${infer Rest}`
    ? `${CapitalizeFirst<First>}${UpperCamelCase<Rest>}`
    : CapitalizeFirst<Value>;

type CamelCase<S extends string> = Uncapitalize<UpperCamelCase<S>>;

type CamelizeObject<T> = {
  [Key in keyof T as Key extends string ? CamelCase<Key> : Key]: Camelize<
    T[Key]
  >;
};

type CamelizeArray<T extends readonly unknown[]> = {
  [Key in keyof T]: Camelize<T[Key]>;
};

type Camelize<T> = T extends Record<string, unknown>
  ? CamelizeObject<T>
  : T extends readonly unknown[]
  ? CamelizeArray<T>
  : T;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<
    Equal<
      Camelize<{
        some_prop: string;
        prop: { another_prop: string };
        array: [
          { snake_case: string },
          { another_element: { yet_another_prop: string } },
          { yet_another_element: string },
        ];
      }>,
      {
        someProp: string;
        prop: { anotherProp: string };
        array: [
          { snakeCase: string },
          { anotherElement: { yetAnotherProp: string } },
          { yetAnotherElement: string },
        ];
      }
    >
  >,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/1383/answer
  > View solutions: https://tsch.js.org/1383/solutions
  > More Challenges: https://tsch.js.org
*/
