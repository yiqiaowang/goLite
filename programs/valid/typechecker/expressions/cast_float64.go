package expression

func main() {
  var i int = 0
  var f float64
  var r rune = ' '
  var b bool = true

  f = float64(0)
  f = float64(' ')
  f = float64(0.0)
  f = float64(true)

  f = float64(i)
  f = float64(f)
  f = float64(r)
  f = float64(b)
}
