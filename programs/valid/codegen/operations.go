package main

func addition(){
	var x int = 10
	var y int = 20

	var a float64 = 10.5
	var b float64 = 20.5
	
	var s string = "foo"
	var t string = "bar"

	println("The following should be '30'")
	println(x+y)
	println("The following should be '31'")
	println(a+b)
	println("The following should be 'foobar'")
	println(s+t)
}

func subtraction(){
	var x int = 10
	var y int = 20

	var a float64 = 10.5
	var b float64 = 20.5
	
	println("The following should be '10'")
	println(y-x)
	println("The following should be '10'")
	println(b-a)
}

func multiplication(){
	var x int = 10
	var y int = 20

	var a float64 = 10.5
	var b float64 = 20.5
	
	println("The following should be '200'")
	println(x*y)
	println("The following should be '215.25'")
	println(a*b)
}

func division(){
	var x int = 10
	var y int = 21

	var a float64 = 10.0
	var b float64 = 21.0
	
	println("The following should be '2'")
	println(y/x)
	println("The following should be '2.1'")
	println(y/x)
	println("The following should be '0.47619047619047616'")
	println(x/y)
}

func modulus(){
	var x int = 10
	var y int = 21

	println("The following should be '1'")
	println(y % x)
}
func main(){
	addition()
	subtraction()
	multiplication()
	division()
	modulus()
}
