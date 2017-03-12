// invalid scoping
package main


func foo() {
	{
		var x int = 100
	}
	x = 10
}

