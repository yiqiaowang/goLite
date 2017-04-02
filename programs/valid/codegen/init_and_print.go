// Check that variable initialization is done correctly.
// This also tests printing of all printable types.
package test

func main(){
	var x int
	var y float64
	var z string
	var foo rune

	println("The following should print '0'.")
	println(x)
	println("The following should print '0'.")
	println(y)
	println("The following should print '' (empty string, aka nothing).")
	println(z)
	println("The following should print '0'.")
	println(foo)

	var a = 10
	var b = 10.1
	var c = "foo"
	var d = 'a'

	println("The following should print '10'.")
	println(a)
	println("The following should print '10.1'.")
	println(b)
	println("The following should print 'foo'.")
	println(c)
	println("The following should print '97'.")
	println(d)


	
	s:=1
	t:=1.1
	u:="foo"
	v:='b'

	
	println("The following should print '1'.")
	println(s)
	println("The following should print '1.1'.")
	println(t)
	println("The following should print 'foo'.")
	println(u)
	println("The following should print '98'.")
	println(v)
}
