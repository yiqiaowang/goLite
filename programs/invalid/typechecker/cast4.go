//typecast trying to cast array to int
package main

func foo() {
	var x int
	var y [3]int

	x = int(y)
	return
}
