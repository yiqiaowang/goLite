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
* Javascript is a relatively simple and small language.


##### Cons
* Performance. While it has decent performance, the choice of a lower level language would have resulted in improved performance.
