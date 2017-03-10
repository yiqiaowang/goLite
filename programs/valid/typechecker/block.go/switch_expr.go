package statements

func nothing() {}

func main() {
  var i int = 0

  switch i {
    case 0, 1: nothing()
    case 2, 3: i--
    default : i++
  }
}
