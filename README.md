

ghci is repl for haskell

* use :t variable to get type
* use :k variable to get kind
* use :l filename to load .hs file into repl

faster docs is accessed by SPC m h H

Write code blocks in `ghci` using `:{` and `:}`.
e.g.
```sh
prelude>:{
  do
    putStrLn "hello world"
:}
```


### Haskell Stack
Recommended tool for beginners to manage tools/dependencies/projects.

### Basics

* blocks are indentation based.
* Space ` ` stands for function application.
* To force execution order, use `()` round parentheses, default execution order is lazy (undetermined depending on runtime)

* function is defined with `=`. i.e.
A function definition:
1. starts with the name of the function,
2. followed by its formal parameters separated by white spaces,
3. an equal sign,
4. and an expression that is the body of the function.

e.g. 
`a b c = d e`

* Function application -- in most cases just the "whitespace operator" --has the highest precedence

* There is no order of execution (lazy)

* Order of evaluation is present in `do`.
