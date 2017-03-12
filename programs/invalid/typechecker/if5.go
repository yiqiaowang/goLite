// second block doesnt type check
package main

func foo() {
	var x int
	if true {
		x = 1
	} else {
		x = 2
		y := "hello"
	}

	y = "world"

	return
}

