package main

func int(x float64) float64 {
  return 42.42
}

func main() {
  var x float64 = int(0.0)

  //should print 42.42
  println(x)
}
