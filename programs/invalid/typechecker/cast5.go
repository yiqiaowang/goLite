//typecast trying to cast array to int
package main

type y struct {}

func foo() {
	var x int
	var z y

	x = int(z)
	return
}
