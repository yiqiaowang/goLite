// post doesnt type check
package main

func foo() {
	var x int
	for x=1 ; true ; x=y {
		x = 10
	}
	return
}
