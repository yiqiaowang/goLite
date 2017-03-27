/*
 * Solves subset sum. Inputs should all be nonnegative integers, or slices with nonnegative
 * integers.
 */
package main

func remove_tail(input []int, length int) []int {

	var out []int
	for i := 0; i < length-1; i++ {
		out = append(out, input[i])
	}

	return out
}

func subset_sum(values []int, size, target int) bool {

	if target < 0 {
		println("Please enter a positive sum target.")
		return false
	}

	if target == 0 {
		return true
	}

	if size == 0 && target != 0 {
		return false
	}

	if values[size-1] > target {
		return subset_sum(values, size-1, target)
	} else {
		return subset_sum(values, size-1, target) || subset_sum(values, size-1, target-values[size-1])
	}
}

func main() {

	/* 
         * There are actually 30 elements in the slice. To increase the computation time, 
         * just increase 'size' or increase 'target'.
         */
	var values []int
	values = append(values, 1)
	values = append(values, 54)
	values = append(values, 2)
	values = append(values, 14)
	values = append(values, 111)
	values = append(values, 7)
	values = append(values, 23)
	values = append(values, 96)
	values = append(values, 46)
	values = append(values, 23)
	values = append(values, 9)
	values = append(values, 69)
	values = append(values, 5)
	values = append(values, 30)
	values = append(values, 100)
	values = append(values, 57)
	values = append(values, 83)
	values = append(values, 35)
	values = append(values, 44)
	values = append(values, 23)
	values = append(values, 71)
	values = append(values, 9)
	values = append(values, 29)
	values = append(values, 5)
	values = append(values, 15)
	values = append(values, 99)
	values = append(values, 67)
	values = append(values, 47)
	values = append(values, 77)
	values = append(values, 89)

	var size int = 28

	var target int = 5711

	println("Target: ", target, " reachable: ", subset_sum(values, size, target))
}
