package statements

type point struct {
  x, y int
}

type point1 point

func main() {
  var arr [0]int
  var slice []int
  var p point
  var alias point1

  var a int
  var b float64
  var c rune
  var d bool
  var e string
  var f [0]int
  var g []int
  var h point
  var i point1

  a, b, c, d, e, f, g, h, i = 0, 0.0, ' ', true, " ", arr, slice, p, alias

  var a1 int = a
  var b1 float64 = b
  var c1 rune = c
  var d1 bool = d
  var e1 string = e
  var f1 [0]int = f
  var g1 []int = g
  var h1 point = h
  var i1 point1 = i
}
