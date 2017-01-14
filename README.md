# Minilambda
### Minimalist functional core programming language, just for fun.

Minilambda is a core functional programming language created in one day for training purposes.

Since it was created in one day, it does not implement additional, fancy features. Nevertheless, you can play around with it and getting an idea about how to define a programming language and write an interpreter for it. Implementation is in Haskell.

## Examples
Here are some code examples which can run in Minilambda:
> 42

> NUM 42

> (9 + 12)

> NUM 21

> (0 < 1)

> BOOL True

> (8 = 9)

> BOOL False

Observe that functions have always one argument and are defined in a lambda-like way:
> fn x. (x + 1)

> FN "x" (SUM (ID "x") (NUM 1))

To have functions with higher arity, just define a function returning a function (which is "the standard way" in functional languages):
> fn x. fn y. (x + y)

> FN "x" (FN "y" (SUM (ID "x") (ID "y")))

And here is how to apply functions (basically, you enclose the function between brackets and leave its actual parameter outside):
> (fn x. (x + 1)) 5

> NUM 6

> ((fn x. fn y. (x + y)) 3 ) 9

> NUM 12

Of course, you have if-then-else statements:
> if (8 < 9) then 42 else 24

> NUM 42

To quit the interpreter, just say "exit":
> exit

> Bye!


## Installation and usage
Access src directory and run make to generate minilambda executable:
> cd src

> make

> ./minilambda
