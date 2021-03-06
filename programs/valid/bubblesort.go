// Bubble sort algorithm
package tests;

func bubbleSort(slice []int, size int) []int {

  var sorted []int;
  for i := 0; i < size; i++ {
  	sorted = append(sorted, slice[i]);
  }

  for i := 0; i < size; i++ {
     for j := 0; j < size-1-i; j++ {
       if sorted[j] > sorted[j+1] {
         temp := sorted[j];
         sorted[j] = sorted[j+1];
         sorted[j+1] = temp;
       }
    }
  }

  return sorted;
}

// Test the algorithm to see if it works
func main() {
	var list []int;
	list = append(list, 7);
	list = append(list, 34);
	list = append(list, 5);
	list = append(list, 0);
	list = append(list, -2);
	list = append(list, 9);
	list = append(list, 8);
	list = append(list, 32);

	var size = 8;
  var sorted = bubbleSort(list, size);

  for i := 0; i < size; i++ {
    println ("value at", i, "is", sorted[i]);
  } 

	return;
}
