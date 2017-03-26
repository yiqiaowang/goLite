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
	z := "foo"
	a := true

	println(int(x))
	println(int(y))
	println(int(z))
	println(int(a))
}

func bool_cast() {
	x := 1
	y := 1.0
	z := "foo"
	a := true

	println(bool(x))
	println(bool(y))
	println(bool(z))
	println(bool(a))
}

func string_cast() {
	x := 1
	y := 1.0
	z := "foo"
	a := true

	println(string(x))
	println(string(y))
	println(string(z))
	println(string(a))
}

func float64_cast() {
	x := 1
	y := 1.0
	z := "foo"
	a := true

	println(float64(x))
	println(float64(y))
	println(float64(z))
	println(float64(a))
}
