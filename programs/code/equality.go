package main

type Point struct {
	x, y int
}

type Circle struct {
	center Point
	radius int
}

func struct_cmp() {
	var c1, c2, c3 Circle

	//circle1
	c1.center.x = 1
	c1.center.y = 2
	c1.radius = 3

	//same as circle2
	c2.center.x = 1
	c2.center.y = 2
	c2.radius = 3

	//different than above circles
	c3.center.x = 2
	c3.center.y = 1
	c3.radius = 3

	//should be true
	println("Expect the following to be true:")
	println(c1 == c2)

	//should be false
	println("Expect the following to be false:")
	println(c1 == c3)

}

func num_cmp() {
	var x, y int
	var foo, bar float64

	x = 0
	y = 1

	foo = 1.234
	bar = 1.234

	println("Expect the following to be false:")
	println(x == y)

	println("Expect the following to be true:")
	println(foo == bar)
}

func str_cmp() {
	var x, y string

	x = "foo"
	y = "bar"

	println("Expect the following to be false:")
	println(x == y)
	
	println("Expect the following to be true:")
	println(x == x)
}

func bool_cmp() {
	println("Expect the following to be false:")
	println(false == true)
	
	println("Expect the following to be true:")
	println(false == false)
}

func main() {
	struct_cmp()
	str_cmp()
	num_cmp()
	bool_cmp()
}
	
