// switch statement as terminating statment
package tests

func foo() int{
	x := 100
	switch x {
	case 10:
		return 10
	case 100:
		return 20
	case 1000:
		return 30
	default:
		return 0
	}
}
