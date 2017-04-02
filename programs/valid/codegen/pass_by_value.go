// Go is pass by value
// Here we test parameters and assignment
package test

func iden(n int) int{
	return n;
}

func return_int_slice() []int {
	var x []int
	return x
}

type x struct {
	y int
}

func alter(a x) {
	a.y = 1000
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


	var derp = return_int_slice()
	derp = append(derp, 10)
	println("The following should print '10'")
	println(derp[0])

	var herp x
	alter(herp)
	println("The following should print '0'")
	println(herp.y)

}
