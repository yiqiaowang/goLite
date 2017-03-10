// first block doesnt type check
package main

func foo() {
	var x int
	if true {
		x = 1.2
	}
	return
}

