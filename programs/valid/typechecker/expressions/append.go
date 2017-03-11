package expressions

type point struct {
  x, y int
}

func main() {
  var slice []point
  var p1, p2 point

  var slice2 []point = append(slice, p1)
  slice2 = append(slice2, p2)
}
