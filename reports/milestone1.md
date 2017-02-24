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
- Wrote some tests for the [Scanner](../src/Scanner.x) and the [Parser](../src/Parser.y)

Thomas
- Wrote the majority of the [Parser](../src/Parser.y)
- Wrote the [Pretty Printer](../src/Pretty.hs)
- Wrote most of the [Language] (../src/Language.hs)

Charlie
- Developed the [test suite](../programs/) and automated testing (programs/)
- Wrote the [Weeder](../src/Weeder.hs)
- Wrote the [compiler wrapper](../src/GoLite.hs) and the [CLI code](../app/Main.hs)


### 3. [Scanner](../src/Scanner.x)
The scanner was written using a fairly standard Alex setup. Some macros were defined for certain character sets to improve ease of use of these sets later on in the scanner. Tokens are recognized via regexes and returned in their respective productions.   

One of the main design decisions taken in the scanner was to explicitly give everything its own token, in as detailed a way as possible. For example, one can define a token, ```BinaryOp```, for all binary operations and store the associated characters with ```BinaryOp```. This does eliminate some initial code repetition, but it introduces plenty of case matching in further stages. While the decision to assign each binary operation its own token has an initial cost, it reduces the cost of all subsequent uses of binary operations.   

Another design decision made in the scanner was to recognize and split up the three different integer representations, assigning each its own token, and parsing all as decimal for use in the rest of the compiler. The decision to split up the integers in the scanner follows the same philosophy as the previously mentioned design decision; we would rather take an initial one time cost and save on all subsequent uses of integers, instead of saving on a one time cost and having more complexity on every use.

The decision to use a monadic scanner is twofold. Coupled with the monadic parser, it allowed us to generate reasonable error messages. Furthermore, the state monad was used to insert optional semicolons into the token stream wherever the conditions to do so were satisfied. This was acheived by storing the previous token and, on newlines, checking to see if the previous token matched one of those outlined in [rule 1](https://golang.org/ref/spec#Semicolons).


### 4. [Parser](../src/Parser.hs)
To make the parser more readable, the tokens were given appropriate aliases, usually just using their names in Go. Precedences were created to allow us to put all the expressions into one data type, without having to seperate them into factors and terms.

A Program data type was used to describe the entire program. It's seperated into the package declaration followed by a list of statements (declarations included as statements). The statement data type was split into three types:

1. All - This can either be any statement, or it can be a function declaration. This is to make it impossible for functions to be declared inside blocks. Only the program data type has a field for an All list.

2. Stmt - This data type contains most of the statement options. It includes if statements, loops, switch statements, variable and type declarations, return statements, print statements, breaks and continues.

3. SimpleStmt - This data type contains statements that can be used before blocks in for loops, if statements, and switch statements

If statements were also given their own data types, to allow them to recursively add if statements like so:
```
if x < 0 {
} else if x < 1 {
} else if x < 2 {
} else if x < 3 {
} else {}
```
Identifiers were given a data type where they could either be a regular identifier (represented by a string), an indexed identifier for arrays and slices, or an identifier with field access for structs.

Types were also given a data type where they could either be a regular type (represented by a string), an array type, a slice type, or a struct type.

A few other data types were created as well for parameters in functions, literal values, clauses in switch statements, variables for variable declarations, and typenames for type declarations.

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
