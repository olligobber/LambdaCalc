# LambdaCalc
Lambda Calculus Tools and Interpreter written in Haskell

# Interpreter
By compiling `Interpreter.hs`, one gets a simple interpreter. The interpreter
takes the entire file, parses it as lambda calculus, and then simplifies it
10,000,000 times, or until it cannot be simplified. Providing an optional
command line argument can increase or decrease the number of times it is
simplified.

# Interface
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
