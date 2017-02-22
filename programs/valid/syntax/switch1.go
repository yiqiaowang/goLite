// default/case/case
package tests;

func main() {
  var tag int = 0;

  switch tag {
    default: tag = 1; tag = 2;
    case tag, 3: tag = 2; tag = 3;
    case tag, 4: tag = 3;
  }
}
