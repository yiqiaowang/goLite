// Calculates the nth fibonacci number, where n is a positive integer (greater than or equal to 1), starting from 0,1,1,2,3,5,...

package fibonacci

func fibonacci (x int) int{
	if x == 1 {
		return 0;
	} else if x == 2 {
		return 1;
	} else {
		return fibonacci(x-1) + fibonacci(x-2)
	}
}
