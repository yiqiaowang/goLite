// Verify the semantics of type casting
package main

type a int
type b a

func complex_cast() {
	//cast 0 to a
	var a1 a = a(0)

	//cast 0 to b
	var b1 b = b(0)

	// cast a, with value 0, to b
	var b2 b = b(a1)

	//should print true
	println("The following should be 'true'.")
	println(b1 == b2)
}

func int_cast() {
	x := 1
	y := 1.0

	println("The following should be '1'")
	println(int(x))
	println("The following should be '1'")
	println(int(y))
		
	// Go doesn't allow for casts from bool to ints
}

func bool_cast() {
	x := 1
	y := 1.0
	a := true
	
	// Go does not allow for type casts from int to bool
	// Golite allows for this via comparison to 0, where 0 is false, rest is true

	println("The following should be 'true'")
	println(bool(x))
	println("The following should be 'true'")
	println(bool(y))
	println("The following should be 'true'")
	println(bool(a))
	
	
	println("The following should be 'false'")
	println(bool(0))
	println("The following should be 'false'")
	println(bool(0.0))
}


func float64_cast() {
	x := 1
	y := 1.0

	println("The following should be '1.0'")
	println(float64(x))
	println("The following should be '1.0'")
	println(float64(y))
	// Go doesn't allow for casts from bool to floats
}


func main() {
	complex_cast()
	string_cast()
	bool_cast()
	int_cast()
	float64_cast()
}
