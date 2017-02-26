// switch statement with break
package tests

func id() int{
	x := 100
	switch x {
	case x < 100:
		break
	case x > 101:
		return 100
	default:
		return 5050
	}
}
