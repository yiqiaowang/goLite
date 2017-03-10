package expressions

type point struct {
  x, y int
}

type point1 point
type point2 point

func f_alias(a, b point2) point1 {
  var c point1
  return c
}

func main() {
  var a, b point2
  var c point1 = f_alias(a, b)
}
