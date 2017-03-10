// body doesnt type check
package main

func foo() {
	var x int
	for {
		x = y
	}
	return
}
