package main

type Point struct {
  x, y int
}

type Circle struct {
  center Point
  radius int
}


func main() {
  var c1, c2, c3 Circle

  //circle1
  c1.center.x = 1
  c1.center.y = 2
  c1.radius = 3

  //same as circle2
  c2.center.x = 1
  c2.center.y = 2
  c2.radius = 3

  //different than above circles
  c3.center.x = 2
  c3.center.y = 1
  c3.radius = 3

  //should be true
  println(c1 == c2)

  //should be false
  println(c1 == c3)

}
