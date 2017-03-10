// stmts dont type check
package main

func foo() {
	var x int
	for x=1 ; true ; x++ {
		x = y 
	}
	return
}
