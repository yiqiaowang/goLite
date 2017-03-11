// arithmetic operations test. function call multiple argument type
package main

func returnInt(x int, y float64) int {
	return 3
}

func foo() {
	var x float64
	var y int
	var z int

	z = returnInt(x,y)
	return
}

