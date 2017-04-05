### Status
[![Build Status](https://api.travis-ci.com/yiqiaowang/comp520-2017-01.svg?token=hr7c7sHjqwUBhqrhRBYy&branch=master)](https://travis-ci.com/yiqiaowang/comp520-2017-01)

# goLite

Team Members:
* Yi Qiao Wang - 260682080
* Charlie Bloomfield - 260520615
* Thomas Jansen - 260554029


--
## MileStones - Please read for notes regarding Milestones
### Milestone 4
* By default, the compiler will do nothing. For functionality, runtime flags may be used. Run ```./run.sh foo.go -h``` to print the help message. There, you will find the flags for additional functions such as typechecking and code generation. IMPORTANT, when passing flags, make sure to include the flags after the input file path. Multiple flags can be used as follows: ```./run.sh foo.go -r -c``` (this pretty prints foo.go to foo.pretty.go [-r] and then generates target code and writes it to foo.js [-c]). When an error occurs, the program exits immediately.

### Milestone 3
* We have included 3 benchmark programs. This is due to the fact that we have already submitted ```dijkstra.go``` in a previous milestone. As of this milestone, ```subset_sum.go``` and ```knapsack.go``` are new programs. We feel like ```dijkstra.go``` is more impressive than either of the other two, however, just in case we are not allowed to submit ```dijkstra.go```, there are still two other benchmark programs.

### Milestone 2
* Question 1: The 20 invalid programs are in the [```types```](programs/invalid/tests/) directory.
* Question 2: _We have implemented the "terminating statements" rules returns._ The compiler will typecheck if invoked with the flag ```-t```: ```stack exec golite-exe -- foo.go -t```. The run script, by default, will run the file with the ```-t``` flag enabled. To run with the ```-pptype``` flag, we can do either of the following: ```./run.sh foo.go -p``` or ```./run.sh foo.go --pptype```. To run with the ```-dumpsymtab``` flag, please use either ```./run.sh foo.go -d``` or ```./run.sh foo.go --dumpsymtab```. 
* Question 3: [Report](doc/milestone2.pdf)


### Milestone 1
* Question 1: The structure of [```programs/```](programs/) looks like

    ```
    programs/
      valid/
        syntax/
        valid_program1.go
        valid_program2.go
      tmp/
      invalid/
        weeder/
        parser/
        invalid_program1.go
        invalid_program2.go
    ```

  For question 1, please grade the 6 golite programs located in the ```valid/``` directory (ie. their paths should be ```programs/valid/*.go```). For the invalid programs, any golite program in any subpath of ```invalid/``` can be graded, however 30 invalid golite programs are provided in the ```invalid/``` directory for convenience (ie. their paths should be ```programs/invalid/*.go```).
* Question 2: If you run out of disk quota when building, it might help to use ```stack.yaml.bak``` instead of ```stack.yaml```. To use ```stack.yaml.bak``` just rename it to ```stack.yaml```.
* Question 3: [Report] (doc/milestone1.pdf)



## Documentation
### 1. Installation
```
git clone https://github.com/Sable/comp520-2017-01
cd comp520-2017-01
stack build
```

--
### 2. Build
The following two commands are equivalent.

`stack build`

OR

`./build.sh`


--
### 3. Test
`stack test`


--
### 4. Usage
Given a set of command line arguments `ARGS`, the following two commands are equivalent.

`./run.sh ARGS`

OR

`stack exec golite-exe -- ARGS`


The following commands are accepted.

```
golite-exe
usage : golite-exe filename [-t] [-d] [-a] [-p] [-h] [--version]

mandatory arguments:
 filename                      goLite source file with relative
                               file path

optional arguments:
 -t, --typecheck               Type checks the input source file
 -d, --dumpsymtab              Dumps the entire symbol table on
                               completion or error.
 -a, --astdump                 Dumps the ast on completed parse
 -p, --pptype                  Pretty prints the program with the
                               type of each expression
 -h, --help                    show this help message and exit
 --version                     print the program version and exit
 ```
