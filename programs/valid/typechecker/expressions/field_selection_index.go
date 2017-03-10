package expressions

type wrapped {
  slice []int
  array [0]int
}

type wrapper {
  inner wrapped
}

type wrapper' wrapper

func main() {
  var w wrapper'

  var x []int = w.inner.slice
  var i int = w.inner.slice[0]

  var y [0]int = w.inner.array
  var j int = w.inner.array[0]

}
