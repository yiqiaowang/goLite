package expressions

type point struct {
  x, y int
}

type point1 point

func computation() point {
  var p1 point1
  return p1
}
