// invalid scoping
package main


func foo() {
	switch x:=10; {
		case true :
		default :
		}
		x = 10
	}
