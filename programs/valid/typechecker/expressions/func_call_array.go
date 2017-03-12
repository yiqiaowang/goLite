package expressions

func f_array(a, b [0]int) [0]int {
  var x [0]int
  return x
}

func main() {
  var a, b [0]int
  var c [0]int = f_array(a, b)
}
