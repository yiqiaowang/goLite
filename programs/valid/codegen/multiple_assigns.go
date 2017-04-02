// This tests the "swap" behaviour found in multiple assigns.
package test


func main(){
	var x int;
	x, y := 10, x;

	println("The following should print '10'.")
	println(x)
	println("The following should print '0'.")
	println(y)


	var a, b, c, d int
	a, b, c, d = 10, 20, 30, 40

	println("The following should print '10'.")
	println(a)
	println("The following should print '20'.")
	println(b)
	println("The following should print '30'.")
	println(c)
	println("The following should print '40'.")
	println(d)
			
			
	a, b, c, d = d, c, b, a


	println("The following should print '40'.")
	println(a)
	println("The following should print '30'.")
	println(b)
	println("The following should print '20'.")
	println(c)
	println("The following should print '10'.")
	println(d)	
}
