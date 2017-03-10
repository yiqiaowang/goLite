// expr does not resolve to bool
package main

func foo() {
	var x int
	for x {
		print(x)
	}
	return
}
