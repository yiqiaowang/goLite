// invalid scoping
package main


func foo() {
	for x:=10; true; {
	}
	x = 100
}
