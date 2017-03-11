package expressions

type point struct {
  x, y int
}

func main() {
  var p1, p2 point

  var a bool = p1 == p2
  var b bool = p1 != p2
}
