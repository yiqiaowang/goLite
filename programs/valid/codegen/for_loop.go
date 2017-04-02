// for loops
package main

func for_loop(){

	println("The following should print out the numbers 1 to 9, each on a different line.")

	for x:=0; x < 10; x++{
		println(x)
	}
}

func for_loop_alt1(){

	println("The following should print out the numbers 1 to 9, each on a different line.")
	var x int
	for ; x < 10; x++{
		println(x)
	}
}

func for_loop_alt2(){

	println("The following should print out the numbers 1 to 9, each on a different line.")
	var x int
	for ; x < 10;{
		println(x)
		x++
	}
}

func while_loop(){

	println("The following should print out the numbers 1 to 9, each on a different line.")

	var x int
	for x < 10 {
		println(x)
		x++
	}
}

func infinite_loop(){

	println("The following should print out the numbers 1 to 9, each on a different line.")

	var x int
	for {
		if (x < 10){
			println(x)
			x++
		} else {
			break
		}
	}
}

func infinite_loop_alt(){

	println("The following should print out the numbers 1 to 9, each on a different line.")

	var x int
	for {
		if (x < 10){
			println(x)
			x++
			continue
		}
		break

	}
}
func main(){
	for_loop()
	for_loop_alt1()
	for_loop_alt2()
	while_loop()
	infinite_loop()
	infinite_loop_alt()
}
