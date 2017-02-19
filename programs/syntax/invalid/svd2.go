// duplicate declaratino
package tests;

func main() {
  a, a := 1, 2; //illegal: double declaration of a or no new variable if a was declared elsewhere
}
