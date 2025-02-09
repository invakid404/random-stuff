/*
  33763 - Union to Object from key
  -------
  by clen cat (@a145789) #hard

  ### Question

  Find the object containing the key in the union type by the key. It  takes two parameters: a union of object types and a key name.

  > View on GitHub: https://tsch.js.org/33763
*/

/* _____________ Your Code Here _____________ */
type UnionToObjectFromKey<Union, Key extends PropertyKey> =
  TypeForKey<Union, Key> extends infer T
    ? Union extends unknown
      ? MyEqual<
          Union,
          Omit<Omit<Union, Key> & { [K in Key]: T }, never>
        > extends true
        ? Union
        : never
      : never
    : never;

type TypeForKey<Union, Key extends PropertyKey> = Union extends unknown
  ? Key extends keyof Union
    ? Union[Key]
    : never
  : never;

type MyEqual<X, Y> =
  (<T>() => T extends X ? 1 : 2) extends <T>() => T extends Y ? 1 : 2
    ? true
    : false;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type Foo = {
  foo: string;
  common: boolean;
};

type Bar = {
  bar: number;
  common: boolean;
};

type Other = {
  other: string;
};

type cases = [
  Expect<Equal<UnionToObjectFromKey<Foo | Bar, 'foo'>, Foo>>,
  Expect<
    Equal<
      UnionToObjectFromKey<Foo | Bar, 'common'>,
      | {
          foo: string;
          common: boolean;
        }
      | {
          bar: number;
          common: boolean;
        }
    >
  >,
  Expect<
    Equal<
      UnionToObjectFromKey<Foo | Bar | Other, 'common'>,
      | {
          foo: string;
          common: boolean;
        }
      | {
          bar: number;
          common: boolean;
        }
    >
  >,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/33763/answer
  > View solutions: https://tsch.js.org/33763/solutions
  > More Challenges: https://tsch.js.org
*/
