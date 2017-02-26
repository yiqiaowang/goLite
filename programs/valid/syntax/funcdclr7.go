// Terminating block as terminating statement
package tests

func foo() int{
	x := 0
	{
		y := 1
		return y
	}
}
