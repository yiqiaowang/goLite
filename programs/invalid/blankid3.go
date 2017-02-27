// blank identifier in array index
package tests

func foo(){
	var a [3]int
	a[0] += a[1+_]
}
