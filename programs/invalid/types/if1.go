// init doesnt type check
package main

func foo() {
	var x int
	if x=y; true {
		x = 10
	}
	return
}

