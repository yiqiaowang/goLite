// append. second arg type not same as slice underlying type
package main

func foo() {
	var a []int
	var b float64

	a = append(a,b)
	return
}

