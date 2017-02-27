// if with else if branch that has a non terminating else
package tests

func foo() int{
	x := 100
	if x < 100 {
		return x
	} else if x > 500 {
		return 500
	} else {
		x = x + 1
	}
}
