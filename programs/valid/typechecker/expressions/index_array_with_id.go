package expressions

func main() {
  var a [1]int
  var index int = 0

  a[index] = 0
  var i int = a[index] // should be 0
}
