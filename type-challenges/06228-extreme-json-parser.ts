/*
  6228 - JSON Parser
  -------
  by Hydration (@hydrati) #extreme #template-literal #json

  ### Question

  You're required to implement a type-level partly parser to parse JSON string into a object literal type.

  Requirements:
   - `Numbers` and `Unicode escape (\uxxxx)` in JSON can be ignored. You needn't to parse them.

  > View on GitHub: https://tsch.js.org/6228
*/

/* _____________ Your Code Here _____________ */
type Whitespace = ' ' | '\t' | '\n';

type Parse<T extends string> = ParseValue<T> extends [infer V, unknown]
  ? V
  : never;

type ParseValue<T> = T extends `${Whitespace}${infer Rest}`
  ? ParseValue<Rest>
  : T extends `true${infer Rest}`
  ? [true, Rest]
  : T extends `false${infer Rest}`
  ? [false, Rest]
  : T extends `null${infer Rest}`
  ? [null, Rest]
  : T extends `"${infer Rest}`
  ? EvalString<Rest>
  : T extends `${'['}${infer Rest}`
  ? EvalArray<Rest>
  : T extends `${'{'}${infer Rest}`
  ? EvalObject<Rest>
  : false;

type Escapes = { r: '\r'; n: '\n'; b: '\b'; f: '\f' };

type EvalString<T, Acc extends string = ''> = T extends `"${infer Rest}`
  ? [Acc, Rest]
  : T extends `\\${infer C extends keyof Escapes}${infer Rest}`
  ? EvalString<Rest, `${Acc}${Escapes[C]}`>
  : T extends `${infer Head}${infer Rest}`
  ? EvalString<Rest, `${Acc}${Head}`>
  : false;

type EvalArray<T, Acc extends any[] = []> = T extends `${Whitespace}${infer U}`
  ? EvalArray<U, Acc>
  : T extends `]${infer Rest}`
  ? [Acc, Rest]
  : T extends `,${infer Rest}`
  ? EvalArray<Rest, Acc>
  : ParseValue<T> extends [infer Value, infer Rest]
  ? EvalArray<Rest, [...Acc, Value]>
  : false;

type EvalObject<
  T,
  CurrentKey extends string = '',
  Acc = {},
> = T extends `${Whitespace}${infer Rest}`
  ? EvalObject<Rest, CurrentKey, Acc>
  : T extends `}${infer Rest}`
  ? [Acc, Rest]
  : T extends `,${infer Rest}`
  ? EvalObject<Rest, CurrentKey, Acc>
  : T extends `"${infer Rest}`
  ? ParseValue<`"${Rest}`> extends [`${infer Key}`, infer Rest]
    ? EvalObject<Rest, Key, Acc>
    : false
  : T extends `:${infer ValueString}`
  ? ParseValue<ValueString> extends [infer Value, infer Rest]
    ? EvalObject<Rest, '', Omit<{ [Key in CurrentKey]: Value } & Acc, never>>
    : false
  : false;

/* _____________ Test Cases _____________ */
import type { Equal, Expect } from '@type-challenges/utils';

type cases = [
  Expect<
    Equal<
      Parse<`
      {
        "a": "b", 
        "b": false, 
        "c": [true, false, "hello", {
          "a": "b", 
          "b": false
        }], 
        "nil": null
      }
    `>,
      {
        nil: null;
        c: [
          true,
          false,
          'hello',
          {
            a: 'b';
            b: false;
          },
        ];
        b: false;
        a: 'b';
      }
    >
  >,
  Expect<Equal<Parse<'{}'>, {}>>,

  Expect<Equal<Parse<'[]'>, []>>,

  Expect<Equal<Parse<'[1]'>, never>>,

  Expect<Equal<Parse<'true'>, true>>,

  Expect<
    Equal<Parse<'["Hello", true, false, null]'>, ['Hello', true, false, null]>
  >,

  Expect<
    Equal<
      Parse<`
      {
        "hello\\r\\n\\b\\f": "world"
      }`>,
      {
        'hello\r\n\b\f': 'world';
      }
    >
  >,

  Expect<Equal<Parse<'{ 1: "world" }'>, never>>,

  Expect<
    Equal<
      Parse<`{ "hello
  
  world": 123 }`>,
      never
    >
  >,
];

/* _____________ Further Steps _____________ */
/*
  > Share your solutions: https://tsch.js.org/6228/answer
  > View solutions: https://tsch.js.org/6228/solutions
  > More Challenges: https://tsch.js.org
*/
