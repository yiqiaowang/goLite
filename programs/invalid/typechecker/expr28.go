// arithmetic operations test. function call argument type
package main

func returnInt(x int) int {
	return x
}

func foo() {
	var x float64
	x = returnInt(3)
	return
}

