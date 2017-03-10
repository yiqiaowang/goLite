package expression

func main() {
  var i int
  var f float64 = 0.0
  var r rune = ' '
  var b bool = true

  i = int(0)
  i = int(' ')
  i = int(0.0)
  i = int(true)

  i = int(i)
  i = int(f)
  i = int(r)
  i = int(b)
}
