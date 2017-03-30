// Check that variable initialization is done correctly.
// This also tests printing of all printable types.
package test

func main(){
	var x int
	var y float64
	var z string
	var foo rune

	println("The following should print '0'.")
	println(x)
	println("The following should print '0'.")
	println(y)
	println("The following should print '' (empty string, aka nothing).")
	println(z)
	println("The following should print '0'.")
	println(foo)
}
