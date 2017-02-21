// Expression function call test
package tests;

func id(x int) int {
	return x;
}

func main() {
	var x int = 10;
	var y int;

	y = id(x);
}
