// Knapsack algorithm
package tests;

// returns the maximum of two numbers
func max(x, y int) int {
  if x > y { 
    return x 
  } else {
    return y
  }
}

// runs the knapsack algorithm
func knapsack(maxW, n int, weights, values []int) int {

  if n == 0 || maxW == 0 {
    return 0;
  }

  if weights[n-1] > maxW {
    return knapsack(maxW, n-1, weights, values);
  }

  return max( (values[n-1] + knapsack(maxW - weights[n-1], n-1, weights, values)),
              (knapsack(maxW, n-1, weights, values)) );
}

// Test the algorithm to see if it works
func exampleTest() int {
  var (
    maxW = 20
    n = 8
    weights, values []int
    )

  weights = append(weights, 3);
  weights = append(weights, 4);
  weights = append(weights, 5);
  weights = append(weights, 6);
  weights = append(weights, 7);
  weights = append(weights, 8);
  weights = append(weights, 9);
  weights = append(weights, 10);

  values = append(values, 1);
  values = append(values, 3);
  values = append(values, 4);
  values = append(values, 4);
  values = append(values, 5);
  values = append(values, 6);
  values = append(values, 9);
  values = append(values, 8);
  
  return knapsack(maxW, n, weights, values);
}