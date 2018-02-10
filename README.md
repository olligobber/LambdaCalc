# LambdaCalc
Lambda Calculus Tools and Interpreter written in Haskell

## Interpreter
By compiling `Interpreter.hs`, one gets a simple interpreter. The interpreter
takes the entire file, parses it as lambda calculus, and then simplifies it
10,000,000 times, or until it cannot be simplified. Providing an optional
command line argument can increase or decrease the number of times it is
simplified.

### Example Usage
```
~$ cat test.lambda
(\p.p (\x.\y.x))
(
  (\x.\y.\z. z x y)
  1
  2
)
~$ ghc Interpreter.hs
~$ cat test.lambda | ./Interpreter 0
(λa.(a λb.λc.b)) ((λa.λb.λc.((c a) b)) 1 2)
~$ cat test.lambda | ./Interpreter 1
((λa.λb.((b 1) a)) 2 λa.λb.a)
~$ cat test.lambda | ./Interpreter
1
~$
```

## Interface
By compiling `Interface.hs`, one gets a command line interface. The interface
has 5 commands, `parse`, `show`, `help`, `interpret` and `quit`. Entering
any word that starts with the same first letter as any of the commands will
run that command.

`help` displays information about the available commands.

`quit` closes the interface.

`parse <lambda>` parses a provided lambda calculus expression and stores it,
overwriting any previously stored expression.
If the provided expression fails to be parsed, the previously stored expression
will remain stored and not be overwritten.

`show` displays the stored expression.

`interpret <times> [<lambda>]` simplifies expressions. The first argument is
the number of times the expression will be simplified, and can be an integer
or `forever` (or anything starting with `f`). Take care using this, as
`interpret forever (\x.x x)(\x.x x)` will never finish.
The second argument is the lambda expression that is to be simplified. If it
is absent or fails to be parsed, the stored lambda expression will be
simplified.

### Example Usage
```
~$ ghc Interface.hs
~$ ./Interface
> parse (\x.\y.\z.z x y) 1 2
Parsed
> interpret 2
Simplified expression 2 time(s).
λa.((a 1) 2)
> i forever
Simplified expression 0 time(s).
λa.((a 1) 2)
> parse (λa.((a 1) 2)) (\x.\y.x)
Parsed
> interpret f
Simplified expression 3 time(s).
1
> i 10000000 (\x.x x)(\x.x x)
Simplified expression 10000000 time(s).
(λa.(a a)) λa.(a a)
> p .Invalid.
Failed to parse
> show
(λa.(a a)) λa.(a a)
> quit
Quitting
~$
```

## Allowed Expressions
Expressions in lambda calculus can be broken up into lambdas, brackets, dots,
variables and spaces. Valid characters for lambdas are `\` and `λ`. Brackets
must be `(` and `)`, and dots must be `.`. A space can be any unicode space
character, including `\t`, `\n`, `\r`, `\f`, `\v`. A variable must consist of
one or more characters, which can be `'`, `"`, `_`,`` ` `` or any alphanumeric
character except `λ`. `a`, `` A` ``, `__var__` and `"你好"` are all valid
variables.

Extra spaces are ignored, and spaces either side of brackets, dots and lambdas
are ignored. For example, `\x.(x y)` and ` \ x . ( x  y ) ` are parsed
identically, but `ab` and `a b` are not.

An expression can be either binding a variable, or applying a variable.
Binding a variable is written as `\v.x`, where `\` can be any lambda, `v`
can be any variable, and `x` can be any expression.
Application can be written with spaces or brackets, such as `f x` or `f(x)`
or even `(f)x`. Application is left associative, so `f x y` is the same as
`(f x) y`. To force right associativity, use brackets, such as `f (x y)`.

The expression after a binding ends only at a close bracket or the end of the
input. This means that `\x.a b c d` will be interpreted as `\x.(a b c d)`.
To apply a variable to a binding, you must bracket the binding, such as
`(\x.x) a`.
