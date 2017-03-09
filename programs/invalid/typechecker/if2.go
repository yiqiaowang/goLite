// expr doesnt type check
package main

func foo() {
	var x int
	if x=10; x {
		x = 10
	}
	return
}

