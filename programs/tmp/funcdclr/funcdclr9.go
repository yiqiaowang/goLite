//switch with non terminating block
package tests
func foo () int {
	x := 100
	switch x {
	case x < 1000:
		return 10
	case x > 1001:
		return 100101
	default:
		x += 1
	}
}
