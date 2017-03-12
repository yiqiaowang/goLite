package expressions

type point struct {
  x, y int
}

type circle struct {
  center point
  radius int
}

func main() {
	var p point
	var x int = p.x
//  var c circle
//  var x int = c.center.x
//  var y int = c.center.y
//  var p point = c.center
//  var r int = c.radius
  return
}
