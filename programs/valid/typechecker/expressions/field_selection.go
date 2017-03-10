package expressions

type point {
  x, y int
}

type circle {
  center point
  radius int
}

func main() {
  var c circle

  var x int = c.center.x
  var y int = c.center.y
  var p point = c.center
  var r int = c.radius
}
