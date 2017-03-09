// expr doesnt type check
package main

func foo() {
	var x int
	for x=1 ; 100 ; x++ {
		x = 10
	}
	return
}
