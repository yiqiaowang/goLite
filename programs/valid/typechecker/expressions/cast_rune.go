package expression

func main() {
  var i int = 0
  var f float64 = 0.0
  var r rune
  var b bool = true

  r = rune(0)
  r = rune(' ')
  r = rune(0.0)
  r = rune(true)

  r = rune(i)
  r = rune(f)
  r = rune(r)
  r = rune(b)
}
