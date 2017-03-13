# Milestone 2 Report
Group comp520-2017-01

### 1. Team Work
Yi Qiao
* Weeding phase for returns
* Symbol table
* Typechecker
* Tests

Thomas



Charlie

### 2. Design Notes

#### A. Weeder 
-- 
As part of the weeding required to verify that functions are properly returned, we have implemented the "Terminating Statements" section of the GoLang [spec](https://golang.org/ref/spec#Terminating_statements). We initially believed that this was the expected implementation for returns, thus, even though it was not required, we have included this implementation. This implementation is recursive in nature, and enumerates all the different cases outlined in the [spec](https://golang.org/ref/spec#Terminating_statements). For example, in the case of ```switch``` statements, a ```switch``` statement is considered to be terminating if it has a ```default``` case, no breaks, and if all of its ```case``` paths are terminating. To check that all of the ```case``` paths are terminating, we would run the same function that ran on the case statement, on the block of statements in each ```case```, returning the result to the parent.

#### B. SymbolTable  
-- 
The symbol table itself was implemented as a stack of frames (denote this stack as the frame stack), where each frame was a hash map that represented the mappings available in the current scope. Bundled along with the stack of frames, were two other stacks. These two other stacks were used to implement the features associated with the ```--dumpsymtab``` and ```--pptype``` flags. 

The first of these two other stacks, denoted as the history stack, was used to store the state of the symbol table at each scope. This was used to implement the ```--pptype``` flag. Due to Haskell's immutability, it was decided that this was the simplest and easiest way, for us, to pass along type information to the pretty printer. This approach has its pros and cons. An advantage is that it required no need to rebuild a typed version of the AST. This, in turn, redueces the amount of code that needed to be written and tested. Futhermore, this approach was also simple to implement. All that is required, is to store a copy of the entire symbol table on every scope exit. However, the approach we took also introduces unnecessary computation; to pretty print a file with type annotations, we type the file twice. 

The second stack was used to store the history of all popped symbol table frames. Every time a frame is popped, we push it to this second stack. At the termination of the typechecking, either on success or failure, print this stack in reverse order. Again, this decision was made due to the constraints that Haskell imposes.


#### C. TypeChecker  
-- 
The typechecker was done in a recursive nature, similar to what we covered in class. For example, statements, while they have no type themselves, are correctly typed if they satisify the rules outlined in the spec. To satisfy the rules, we might need to type sub-components of the statement, and these are handled by the same recursive function that is typing the statement. Likewise, expressions are well typed if their arguments are well typed, their arguments are well typed if the sub arguments are well typed. This propagates down to the base cases of the type system, things are trivially well typed.


### 3. Invalid Programs
--
Below, we give explainations as to why each program in [types](../../../programs/invalid/types) is incorrectly typed.
