// no return outside of conditional
package tests;

func id(x int, y int) {
  x := 0;
  if x {
    return;
  }
  for {
    return;
  }
}
