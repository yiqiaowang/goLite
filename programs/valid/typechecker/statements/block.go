package statements

func main() {
  var x int = 0

  {
    var y int = 0

    x++
    y++
    {
      x++
      y++
    }

    x--
    y--
  }
  x--
}
