// Verify the semantics of identifiers
package main


// types are not reserved, and thus can be used as identifiers
func int(x float64) float64 {
	return 42.42
}

func rename() {
	var x float64 = int(0.0)

	//should print 42.42
	println("The following should print '42.42'")
	println(x)
}
// values of identifiers can be used in expressions
func expr() {
	var x float64 = 10.10

	println("The following should print '20.20'")
	println(x + 10.10)
}

// identifiers can have their values changed
func change_value() {
	var x float64 = 10.10
	println("The following should print '10.10'")
	println(x)

	x = 20.10
	println("The following should print '20.10'")
	println(x)
}

func main() {
	rename()
	expr()
	change_value()
}
