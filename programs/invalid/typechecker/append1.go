// append first arg not array or slice
package main

func foo() {
	var a int
	var b int
	var x []int

	x = append(a,b)
	return
}

