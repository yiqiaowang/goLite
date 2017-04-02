// Tests bitwise operations
/*
&    bitwise AND            integers
|    bitwise OR             integers
^    bitwise XOR            integers
&^   bit clear (AND NOT)    integers

<<   left shift             integer << unsigned integer
>>   right shift            integer >> unsigned integer
*/

package main

func main(){
	var x int = 110011
	var y int = 100100110


	println("The following should be '75786'")
	println(y & x)
	println("The following should be '100134335'")
	println(y | x)
	println("The following should be '100058549'")
	println(y ^ x)
	println("The following should be '100024324'")
	println(y &^ x)
	println("The following should be '3437'")
	println(x >> 5)
	println("The following should be '3520352'")
	println(x << 5)
}
