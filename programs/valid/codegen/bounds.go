// this tests the outofbounds runtime panic
package main

func main(){
	var x [3]int


	println(x[5])
}
