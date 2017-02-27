// Calculates base ^ exponent by fast exponentiation
// Both base and exponent must be non negative integers
package fast_exp

func fast_exp (x int, y int) int {
	if y == 0 {
		return 1;
	} else if y == 1 {
		return x;
	} else if (y % 2) == 0 {
		return fast_exp(x,y/2) * fast_exp(x,y/2);
	} else {
		return x * fast_exp(x,(y-1)/2) * fast_exp(x,(y-1)/2);
	};
}
