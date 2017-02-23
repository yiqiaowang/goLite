# Milestone 1 Report

### 1. Framework
We are developing our compiler with a Haskell based toolchain.

- [Alex](https://www.haskell.org/alex/)
- [Happy](https://www.haskell.org/happy/)
- [argparser](http://hackage.haskell.org/package/argparser)

### 2. Team Work
Steven
- Developed the [Scanner](../src/Scanner.x)
- Contributed to the development of the [Parser](../src/Parser.y)


Thomas
- Wrote the majority of the [Parser](../src/Parser.y)
- Wrote the [Pretty Printer](../src/Pretty.hs)

Charlie
- Developed the [test suite](../programs/) and automated testing (programs/)
- Wrote the [Weeder](../src/Weeder.hs)
- Wrote the [compiler wrapper](../src/GoLite.hs) and the [CLI code](../app/Main.hs)


### 3. Scanner


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
