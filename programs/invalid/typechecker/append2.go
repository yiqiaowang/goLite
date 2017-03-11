// append. second arg type not same as array underlying type
package main

func foo() {
	var a [10]int
	var b float64

	a = append(a,b)
	return
}

