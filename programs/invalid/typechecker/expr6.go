// arithmetic operations test. can't divide int and rune
package main

func foo() {
	var x int
	var y rune
	x = x / y
	return
}

