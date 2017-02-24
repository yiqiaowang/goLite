# Milestone 1 Report

### 1. Framework
We are developing our compiler with a Haskell based toolchain.

- [Alex](https://www.haskell.org/alex/)
- [Happy](https://www.haskell.org/happy/)
- [argparser](http://hackage.haskell.org/package/argparser)

### 2. Team Work
Yi Qiao
- Developed the [Scanner](../src/Scanner.x)
- Contributed to the development of the [Parser](../src/Parser.y)
- Wrote some tests for the scanner and the parser.

Thomas
- Wrote the majority of the [Parser](../src/Parser.y)
- Wrote the [Pretty Printer](../src/Pretty.hs)

Charlie
- Developed the [test suite](../programs/) and automated testing (programs/)
- Wrote the [Weeder](../src/Weeder.hs)
- Wrote the [compiler wrapper](../src/GoLite.hs) and the [CLI code](../app/Main.hs)


### 3. Scanner
The scanner was written using a fairly standard Alex setup. Some macros were defined for certain character sets to improve ease of use of these sets later on in the scanner. Tokens are recognized via regexes and returned in their respective productions.   

One of the main design decisions taken in the scanner was to explicitly give everything its own token, in as detailed a way as possible. For example, one can define a token, ```BinaryOp```, for all binary operations and store the associated characters with ```BinaryOp```. This does eliminate some initial code repetition, but it introduces plenty of case matching in further stages. While the decision to assign each binary operation its own token has an initial cost, it reduces the cost of all subsequent uses of binary operations.   

Another design decision made in the scanner was to recognize and split up the three different integer representations, assigning each its own token, and parsing all as decimal for use in the rest of the compiler. The decision to split up the integers in the scanner follows the same philosophy as the previously mentioned design decision; we would rather take an initial one time cost and save on all subsequent uses of integers, instead of saving on a one time cost and having more complexity on every use.

The decision to use a monadic scanner is twofold. Coupled with the monadic parser, it allowed us to generate reasonable error messages. Furthermore, the state monad was used to insert optional semicolons into the token stream wherever the conditions to do so were satisfied. This was acheived by storing the previous token and, on newlines, checking to see if the previous token matched one of those outlined in [rule 1](https://golang.org/ref/spec#Semicolons).


### 4. Parser


### 5. [Weeder](../src/Weeder.hs)
The weeder is responsible for verifying the syntactic validity of a small set of constructs that are difficult to verify directly in the parser's CFG. We defer the following verifications to the weeding phase.

- Verifying that there is at most 1 `default` clause per `switch` statement.

- Verifying that there are an equal number of identifiers and strings on each side of a multiple assignment operation.
```
var a, b = 1, 2, 3;
a, b := 1;
```

- Verifying that no identifier is repeated in a multiple short variable declaration.
```
a, a := 1, 2;
```

- Verifying that `break` and `continue` statements only occur inside of loop constructs.
```
func main() {
  break;
  continue;
}
```

- Verifying that `return` statements only occur inside of function declarations.
```
package main;

return;
```

- Verifying that functions have a return statement.
```
func num() int {

}
```

- Verifying that the post statement in a for loop is valid.
```
for i := 0; i < 10; i := 20 {}
```


### 6. Pretty Printer
