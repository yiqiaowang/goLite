// arithmetic operations test. function call return type
package main

func return3() float64 {
	return 1.2345 
}

func foo() {
	var x int
	x = x + return3()
	return
}

