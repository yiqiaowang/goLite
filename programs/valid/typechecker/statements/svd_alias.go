package statements

type point struct {
  x, y int
}

type point1 point

func main() {
  var p point1
  a := p
  var a1 point1 = a
}
