/* 
 * The integer knapsack problem solved via dynamic programming. This prints the entire subproblem matrix.
 * To increase running time, simply increase the capacity of the bag (easy), or add additional values and weights.
 */
 
package main

func maximum(a, b int) int {
	if a > b {
		return a
	} else {
		return b
	}
}

func dynamic_matrix(x, y int) [][]int {
	var matrix [][]int

	for i := 0; i < x; i++ {
		var row []int
		matrix = append(matrix, row)

		for j := 0; j < y; j++ {
			matrix[i] = append(matrix[i], 0)
		}
	}

	return matrix
}

func knapsack(values, weights []int, items, cap int) {
	var matrix [][]int
	matrix = dynamic_matrix(items, cap)

	for i := 0; i < cap; i++ {
		matrix[0][i] = 0
	}

	for i := 1; i < items; i++ {
		for j := 0; j < cap; j++ {
			if weights[i] > j {
				matrix[i][j] = matrix[i-1][j]
			} else {
				matrix[i][j] = maximum(matrix[i-1][j], matrix[i-1][j-weights[i]]+values[i])
			}

		}
	}

	for i := 0; i < items; i++ {
		for j := 0; j < cap; j++ {
			println("The maximum weight from the first ", i+1, " items given a knapsack of capacity ", j, " is ", matrix[i][j])
		}
	}
}

func main() {
	var values []int
	values = append(values, 1)
	values = append(values, 2)
	values = append(values, 3)
	values = append(values, 4)
	values = append(values, 5)
	values = append(values, 6)
	values = append(values, 7)
	values = append(values, 8)
	values = append(values, 9)
	values = append(values, 10)
	values = append(values, 11)
	values = append(values, 12)
	values = append(values, 13)
	values = append(values, 14)
	values = append(values, 15)
	values = append(values, 16)
	values = append(values, 17)
	values = append(values, 18)
	values = append(values, 19)
	values = append(values, 20)

	var weights []int
	weights = append(weights, 2)
	weights = append(weights, 4)
	weights = append(weights, 6)
	weights = append(weights, 8)
	weights = append(weights, 10)
	weights = append(weights, 1)
	weights = append(weights, 3)
	weights = append(weights, 5)
	weights = append(weights, 7)
	weights = append(weights, 9)
	weights = append(weights, 20)
	weights = append(weights, 18)
	weights = append(weights, 16)
	weights = append(weights, 14)
	weights = append(weights, 12)
	weights = append(weights, 11)
	weights = append(weights, 13) 
	weights = append(weights, 15)
	weights = append(weights, 17)
	weights = append(weights, 19)

	knapsack(values, weights, 20, 400)
}
