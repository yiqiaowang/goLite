package expression

func main() {
  var i int = 0
  var f float64 = 0.0
  var r rune = ' '
  var b bool

  b = bool(0)
  b = bool(' ')
  b = bool(0.0)
  b = bool(true)

  b = bool(i)
  b = bool(f)
  b = bool(r)
  b = bool(b)
}
