### Status
[![Build Status](https://api.travis-ci.com/yiqiaowang/comp520-2017-01.svg?token=hr7c7sHjqwUBhqrhRBYy&branch=master)](https://travis-ci.com/yiqiaowang/comp520-2017-01)

# goLite

Team Members:
* Yi Qiao Wang - 260682080
* Charlie Bloomfield - 260520615
* Thomas Jansen - 260554029


---
### 1. Reports
[Milestone 1](reports/milestone1.md)


---
### 2. About


---
### 3. Installation
```
git clone https://github.com/Sable/comp520-2017-01
cd comp520-2017-01
stack build
```

---
### 4. Build
The following two commands are equivalent.

`stack build`

OR

`./build.sh`


---
### 5. Test
`stack test`


---
### 6. Usage
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
