package statements

type point struct {
  x, y, int
}

type point1 point

func main() {
  var arr [0]int
  var slice []int
  var p point
  var p1 point1

  print()
  print(0, 0.0, true, false, ' ', " ", ` `, arr, slice, p, p1)
}
