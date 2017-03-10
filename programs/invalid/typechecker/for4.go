// init doesnt type check
package main

func foo() {
	var x int
	for x=1.5 ; true ; x++ {
		x = 10
	}
	return
}
