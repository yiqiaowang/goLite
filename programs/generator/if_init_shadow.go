// Verify that the optional short variable declaration in if
// statements has the correct semantics
package main

func main() {
	var x string = "root"

	//init x as int, validate it exists in if block
	if x := 0; true {

		//init x as float64, validate it exists in else block
		if x := 1.0; false {
			//do nothing
		} else {
			//should print 1.0
			println("The following should be '1.0'")
			println(x)
		}

		//should print 1
		println("The following should be '1'")
		println(x)
	}

	//should print "root"
	println("The following should be 'root'")
	println(x)
}
