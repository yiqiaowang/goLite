// This tests the "swap" behaviour found in multiple assigns.
package test

func main(){
	var x int;
	x, y := 10, x;

	println("The following should print '10'.")
	println(x)
	println("The following should print '0'.")
	println(y)
}
