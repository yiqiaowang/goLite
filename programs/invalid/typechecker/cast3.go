//typecast trying to cast slice to int
package main

func foo() {
	var x int
	var y []int

	x = int(y)
	return
}
