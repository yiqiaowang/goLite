// body doesnt type check
package main

func foo() {
	var x int
	for true {
		x = y
	}
	return
}
