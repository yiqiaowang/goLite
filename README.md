### Status
[![Build Status](https://api.travis-ci.com/yiqiaowang/comp520-2017-01.svg?token=hr7c7sHjqwUBhqrhRBYy&branch=master)](https://travis-ci.com/yiqiaowang/comp520-2017-01)

# goLite

Team Members:
* Yi Qiao Wang - 260682080
* Charlie Bloomfield - 260520615
* Thomas Jansen - 260554029


--
## MileStones
### Milestone 1
* Question 1: The structure of [programs](programs/) looks like 

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
* Question 3: [Report] (reports/milestone1.md) 



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
usage : golite-exe filename [-h] [--version]

mandatory arguments:
 filename                      goLite source file with relative
                               file path

optional arguments:
 -h, --help                    show this help message and exit
 --version                     print the program version and exit
 ```
