// Tests appends on slices and arrays.
package main

func slice_test(){
	var x []int

	x = append(x,10)
	x = append(x,20)
	x = append(x,30)

	println("The following should be '10'")
	println(x[0])
	println("The following should be '20'")
	println(x[1])
	println("The following should be '30'")
	println(x[2])
}

func array_test(){
	var x [3]int

	x[0] = 10
	x[1] = 20 
	x[2] = 30

	println("The following should be '10'")
	println(x[0])
	println("The following should be '20'")
	println(x[1])
	println("The following should be '30'")
	println(x[2])
}

func main(){
	slice_test()
	array_test()
}
