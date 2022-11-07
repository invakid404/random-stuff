/*
  956 - DeepPick
  -------
  by hiroya iizuka (@hiroyaiizuka) #hard #deep
  
  ### Question
  
  Implement a type DeepPick, that extends Utility types `Pick`.
  A type takes two arguments.
  
  
  For example:
  
  ```
  
  type obj = {
    name: 'hoge', 
    age: 20,
    friend: {
      name: 'fuga',
      age: 30,
      family: {
        name: 'baz',  
        age: 1 
      }
    }
  }
  
  type T1 = DeepPick<obj, 'name'>   // { name : 'hoge' }
  type T2 = DeepPick<obj, 'name' | 'friend.name'>  // { name : 'hoge' } & { friend: { name: 'fuga' }}
  type T3 = DeepPick<obj, 'name' | 'friend.name' |  'friend.family.name'>  // { name : 'hoge' } &  { friend: { name: 'fuga' }} & { friend: { family: { name: 'baz' }}}
  
  ```
  
  > View on GitHub: https://tsch.js.org/956
*/

/* _____________ Your Code Here _____________ */
type SplitKey<Key extends string> = Key extends `${infer Head}.${infer Rest}`
  ? [Head, Rest]
  : [Key, never];

type PickEach<T, Key extends string> = Key extends any
  ? SplitKey<Key> extends [
      infer Head extends string,
      infer Rest extends string | never,
    ]
    ? Head extends keyof T
      ? Pick<T, Head> extends infer Result
        ? [Rest] extends [never]
          ? Result
          : { [Key in keyof Result]: PickEach<Result[Key], Rest> }
        : never
      : never
    : never
  : never;

type UnionToIntersection<U> = (
  U extends any ? (arg: U) => any : never
) extends (arg: infer I) => void
  ? I
  : never;

type DeepPick<T, Key extends string> = UnionToIntersection<PickEach<T, Key>>;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type Obj = {
  a: number;
  b: string;
  c: boolean;
  obj: {
    d: number;
    e: string;
    f: boolean;
    obj2: {
      g: number;
      h: string;
      i: boolean;
    };
  };
  obj3: {
    j: number;
    k: string;
    l: boolean;
  };
};

type cases = [
  Expect<Equal<DeepPick<Obj, ''>, unknown>>,
  Expect<Equal<DeepPick<Obj, 'a'>, { a: number }>>,
  Expect<Equal<DeepPick<Obj, 'a' | ''>, { a: number } & unknown>>,
  Expect<
    Equal<DeepPick<Obj, 'a' | 'obj.e'>, { a: number } & { obj: { e: string } }>
  >,
  Expect<
    Equal<
      DeepPick<Obj, 'a' | 'obj.e' | 'obj.obj2.i'>,
      { a: number } & { obj: { e: string } } & { obj: { obj2: { i: boolean } } }
    >
  >,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/956/answer
  > View solutions: https://tsch.js.org/956/solutions
  > More Challenges: https://tsch.js.org
*/
