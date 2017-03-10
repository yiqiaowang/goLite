package expressions

type point struct {
  x, y int
}

func f_alias(a, b point) point {
  var c point
  return c
}

func main() {
  var a, b point
  var c point = f_alias(a, b)
}
