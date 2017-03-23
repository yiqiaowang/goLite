package main

func main() {
  var a []int

  a = append(a, 0)
  a = append(a, 1)
  a = append(a, 2)

  //should print 1 2 3 on separate lines
  for i := 0; i < 3; i++ {
    println(a[i])
  }
}
