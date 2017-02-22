// nested if/else if/else
package tests;

var x int = 0;

func main() {
  if x {
    if x {}
  } else if x {
    if x {} else {}
  } else {
    if x {} else if x {} else {}
  }
}
