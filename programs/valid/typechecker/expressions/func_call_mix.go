package expressions

type point struct {
  x, y int
}

type point1 point

func f_mix(a int, b bool, c float64, d rune, e string, f []int, g [0]int, h point, i point1) {
  //do nothing
}

func main() {
  var a []int
  var b [0]int
  var p point
  var p1 point1

  f_mix(0, true, 0.0, ' ', " ", a, b, p, p1)
}
