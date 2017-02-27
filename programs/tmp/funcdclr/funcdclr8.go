// switch no default
package tests
func foo() int {
	x := 10000
	switch x {
	case x < 10000:
		return 100034
	case x > 50000:
		return 123
	}
}
