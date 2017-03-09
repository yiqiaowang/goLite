// accessing variable in different block
package main

func foo() int {
	{
		x:=5
	}

	return x
}
