package expressions

type point struct {
  x, y int
}

type point1 point
type point2 point1

func main() {
  var p1, p2 point2

  var a bool = p1 == p2
  var b bool = p1 != p2
}
