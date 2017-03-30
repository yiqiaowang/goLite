// Go is pass by value
// Here we test parameters and assignment
package test

func iden(n int) int{
	return n;
}

func main(){
	var foo int = 100;
	var bar = iden(foo);
	foo = 1337;

	println("The following should print '100'.")
	println(bar);


	bar = foo;
	foo = 100;
	
	println("The following should print '1337'.")
	println(bar);
}
