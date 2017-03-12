package expressions

type wrapped struct {
  slice []int
  array [0]int
}

type wrapper struct {
  inner wrapped
}

type wrapper2 wrapper

func main() {
  var w wrapper2

  var x []int = w.inner.slice
  var i int = w.inner.slice[0]

  var y [0]int = w.inner.array
  var j int = w.inner.array[0]

}
