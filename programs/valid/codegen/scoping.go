// Tests scoping rules
// These should be the same/similar to that found in javascript if we use the 'let' keyword

package test

func main(){
	var x int = 10;
	{
		var x int = 20;
		println("The following should print '20'.")
		println(x)
	}

	println("The following should print '10'.")
	println(x);
}
