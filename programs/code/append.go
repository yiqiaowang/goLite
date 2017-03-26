// Verify the behavior of append
package main

func main() {
	append_int()
	append_string()
	append_float64()
	append_bool()
}

func append_int() {
	var a []int

	a = append(a, 0)
	a = append(a, 1)
	a = append(a, 2)

	//should print 1 2 3 on separate lines
	println("The following should be '1', '2', and '3', each on a separate line.")
	for i := 0; i < 3; i++ {
		println(a[i])
	}
}

func append_string() {
	var a []string

	a = append(a, "foo")
	a = append(a, "bar")
	a = append(a, "herp")

	//should print 1 2 3 on separate lines
	println("The following should be 'foo', 'bar', and 'herp', each on a separate line.")
	for i := 0; i < 3; i++ {
		println(a[i])
	}
}

func append_float64() {
	var a []int

	a = append(a, 0.0)
	a = append(a, 1.0)
	a = append(a, 2.0)

	//should print 1 2 3 on separate lines
	println("The following should be '1.0', '2.0', and '3.0', each on a separate line.")
	for i := 0; i < 3; i++ {
		println(a[i])
	}
}

func append_bool() {
	var a []int

	a = append(a, true)
	a = append(a, false)

	//should print 1 2 3 on separate lines
	println("The following should be 'true' and 'false', each on a separate line.")
	for i := 0; i < 2; i++ {
		println(a[i])
	}
}
