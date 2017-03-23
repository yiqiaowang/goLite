package main

type a int
type b a

func main() {
  //cast 0 to a
  var a1 a = a(0)

  //cast 0 to b
  var b1 b = b(0)

  // cast a, with value 0, to b
  var b2 b = b(a1)

  //should print true
  println(b1 == b2)
}
