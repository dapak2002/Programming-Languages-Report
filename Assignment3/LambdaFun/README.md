```
  $$$$\        $$$$$$$$\                     
    $$ \       $$  _____|                    
     $$ \      $$ |    $$\   $$\ $$$$$$$\  
     $$$ \     $$$$$\  $$ |  $$ |$$  __$$\ 
    $$ $$ \    $$  __| $$ |  $$ |$$ |  $$ |
   $$ / $$ \   $$ |    $$ |  $$ |$$ |  $$ |
 $$$ /   $$$\  $$ |    \$$$$$$  |$$ |  $$ |
 \___|   \___| \__|     \______/ \__|  \__|
```

Instructions:
- Copy this folder to your machine.
- Compile with `cabal build`. 
- If the previous step does not work
  - use docker with [my dockerfile](https://hackmd.io/@alexhkurz/HJxD19_Vo#Assignment-3)
  - or use [`ghcup tui`](https://www.haskell.org/ghcup/install/) to select version 8.10.7 of GHC.
- Launch the REPL with `cabal exec lamfun`. 
- For help, type `:help` in the REPL.
- `:load test/examples.lc`


The full grammar of λFun is given below:

```
<statement> ::= <expr> ";;" | <defn> ";;"
<defn>      ::= "val" <id> "=" <expr> | "rec" <id> " " <ids> "=" <expr>
<expr>      ::= <id> | 
                <number> | 
                "true" | 
                "false" | 
                "\" <ids> "." <expr> | 
                <expr> " " <expr> | 
                <expr> <op> <expr> | 
                "[" <list> "]" | 
                <expr> ":" <expr> |
                "let " <defn> " in " <expr> | 
                "case " <expr> " of " "{" <case> "}" |
                "while " <expr> " do " <expr> | 
                <expr> ";" <expr> |
                <expr> ":=" <expr> | 
                "!" <expr> | 
<list>      ::= "" | <expr> "," <list>
<expr_opt>  ::= "_" | <expr>
<case>      ::= <expr_opt> "->" <expr> | <expr_opt> "->" <expr> "," <case>
<id>        ::= <letter> | <id> <letter> | <id> <digit>
<ids>       ::= <id> | <id> " " <ids>
<op>        ::=  "*" | "+" | "-" | "/" | ">" | ">=" | "==" | "!=" | "=<" | "<"
<number>    ::= <digit>+
<string>    ::= """ (<letter>|<digit>)* """
```

Remark: 

- If in doubt about the syntax  
  - check with the grammar above
  - experiment by asking the REPL (which works similar to ghci in Haskell)
  - use `:tree` to see the abstract syntax tree.
- Comments are as in Haskell.
- `while a do b ; c` is parsed as `(while a do b) ; c`, not as `while a do (b ; c)`
- In `while a do b` the condition `a` should not be in parentheses.
- Definitions are known globally throughout the program. `val` defines a value, including non recursive functions. `rec` defines a recursive function. `let`-expressions are used to make local definitions with limited scope.
- There are built-in functions defined in all environments as follows.

  - One for each `<op>`.
  - A function `new` to allocate memory.
  - Functions `head` and `tail` to take the head or tail of a list.
  - A function `addr` that converts a value (number) into an address.

## Lab2

See [`test/examples.lc`](test/examples.lc) for examples to get started.
