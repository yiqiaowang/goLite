# Milestone 2 Report
Group comp520-2017-01

### 1. Team Work
Yi Qiao
* Fixing bugs from milestone 2
* Writing report

Thomas

Charlie

### 2. Design Notes

#### Target Language  

The language our compiler targets is javascript. This decision was made for the following reasons.
* Javascript is ubiquitous. We want our compiled code to be used in many places.
* Javascript is high level. We wanted to avoid the tasks required to target lower level languages (manual memory management etc.)

The pros and cons of targeting Javascript.

##### Pros
* Javascript is high level. We avoid the somewhat tedious task of manual memory management. This also avoids certain bugs and errors that are unique to targeting a lower level language like C or an assembly language.
* Javascript has language features that play well with those found in Go/Golite. The behaviour of many of the Go/Golite language features are similiar, if not identical, to their counterparts in Javascript.


##### Cons
* Performance. The choice of a lower level language would have resulted in improved performance.
* Scoping rules. Javascript variable declarations with the keyword ```var``` are function scope, and not block scope as they should be. While there is an easy fix (in modern Javascript) by replacing ```var``` with ```let```, we did not discover this issue until much time had been spent debugging seemingly correct generated code.
* Lack of a ```Simple Statement``` in loops. Javascript, unlike Go/Golite, does not have the optional initialization statement in the ```if``` and ```switch``` structures.

### 3. Progress Report

### 4. Description of Programs
