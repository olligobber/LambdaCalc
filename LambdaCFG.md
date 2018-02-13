# Lambda Calculus CFG
A Context Free Grammar (CFG) for Lambda Calculus Expressions.

## Character Sets
In this document, `s` will represent the set of whitespace characters,
including `\t`, `\n`, `\r`, `\f`, and `\v`.
`l` will represent the set of lambda characters; `\` and `λ`.
`v` will represent the set of variable characters, `_`, `'`, `"`, `` ` ``,
and all alphanumeric unicode characters except `λ`. A full list of such
characters can be found in Haskell by importing `Data.Char` and then evaluating
`filter isAlphaNum [minBound..]`.

## Pre-Processing
Before parsing a string using this CFG, it is expected that all whitespace
characters that are either side of a bracket `(` or `)`, a dot `.`, or a
lambda `\` or `λ`, will be stripped, that whitespace at the start and end of
the string be stripped, and that there are no consecutive whitespace
characters. In `ParseLambda.hs`, applying
`filterWhitespace . dropWhile isSpace` to the string will achieve this.

## Simple CFG
The following CFG produces all acceptable lambda expressions:
```
A -> l V . A
A -> V ( A )
A -> V s A
A -> ( A ) A
A -> ( A )
A -> V
V -> v V
V -> v
```

## LL(1) CFG
To parse expressions, we want an LL(1) CFG. The following CFG produces all
acceptable lambda expressions and is LL(1):
```
A -> l V . A    (0)
A -> V B        (1)
A -> ( A ) C    (2)
B -> l V . A    (0)
B -> s A        (3)
B -> ( A ) C    (2)
B -> Null       (4)
C -> l V . A    (0)
C -> V B        (1)
C -> ( A ) C    (2)
C -> Null       (4)
V -> v W        (5)
W -> v W        (5)
W -> Null       (6)
```
The production rules are numbered according to their index in the list
`prodRules` in `ParseLambda.hs`. Below is the LL(1) lookup table for the CFG:

|     | `v` | `(` | `s` | `l` | EOF | `)` | `.` |
| :-: | :-: | :-: | :-: | :-: | :-: | :-: | :-: |
| `A` | 1   | 2   |     | 0   |     |     |     |
| `B` |     | 2   | 3   | 0   | 4   | 4   |     |
| `C` | 1   | 2   |     | 0   | 4   | 4   |     |
| `V` | 5   |     |     |     |     |     |     |
| `W` | 5   | 6   | 6   | 6   | 6   | 6   | 6   |
