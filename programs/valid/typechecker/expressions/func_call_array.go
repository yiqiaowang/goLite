package expressions

func f_array(a, b [0]int) [0]int {
  var a [0]int
  return a
}

func main() {
  var a, b [0]int
  var c [0]int = f_array(a, b)
}
