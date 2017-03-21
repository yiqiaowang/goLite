/*
* This program takes calculates the longest collatz sequence
* for values under 1M. It takes approximates .4s when executed
* with the canonical Go compiler. 
*/
package main

//returns collatz value of x
func collatz(x int) int {
  var isEven bool = (x % 2) == 0

  if isEven {
    return x / 2
  } else {
    return (x * 3) + 1
  }
}

//returns length of collat sequence starting at x
func collatzSequenceLength(x int) int {
  var seqLength int = 1

  for x != 1 {
    x = collatz(x)
    seqLength ++
  }

  return seqLength
}

//calculates longest collatz sequence under 1,000,000
func main() {
  //find longest collatz sequence under 1,000,000
  var maxSequenceLength int = 0
  var maxSequenceValue int

  for i := 1; i < 1000000; i++ {
    var l int = collatzSequenceLength(i)
    if l > maxSequenceLength {
      maxSequenceLength = l
      maxSequenceValue = i
    }
  }

  println("Max sequence value is: ", maxSequenceValue)
  println("Max sequence length is: ", maxSequenceLength)
}
