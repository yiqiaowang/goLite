package statements

func f() {}

func main() {
  var x int
  if f(); true {
    x++
  }
}
