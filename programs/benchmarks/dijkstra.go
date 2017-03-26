/* To increase the running time of this algorithm, increase the size of the graph */

// A completely customizable Random Number Generator
package main;


// sets the value at the given index from an array to negative 1 and returns a new array
func setMinusOne(array []int, size, index int) []int {
  var ret []int;
  for i := 0; i < size; i++ {
    if i != index {
      ret = append(ret, array[i]);
    } else {
      ret = append(ret, -1);
    }
  }
  return ret;
}

// returns a number larger than the combination of all edge lengths in a graph
// useful for a pseudo-infinite number in the dijkstra algorithm
func getMax(graph [][]int, nodes int) int {

  var max int = 0;

  for i := 0; i < nodes; i++ {
    for j := 0; j < nodes; j++ {
      if graph[i][j] < 0 {
        continue;
      } else {
        max += graph[i][j];
      }
    }
  }
  return max*2;
}

/* Dijkstra's algorithm, returns a list of shortest distances to each vertex
 * in the given graph.
 * If a vertex is unreachable, -1 is returned in the index for that vertex.
 * The graph should be a square matrix where each index represents the weight of an
 * edge between the vertices represented by the indices of the matrix.
 * Therefore, the matrix should be symmetric across the main diagonal.
 * If an edge doesn't exist between two vertices, -1 should be put at that edge.
 * The edges cannot have negative weight.
 * Nodes represents the number of vertices in the graph.   */
func dijkstra(graph [][]int, nodes, source int) []int {

  var vertices []int;
  var distances []int;

  max := getMax(graph, nodes);

  for i := 0; i < nodes; i++ {
    distances = append(distances, max);
    vertices = append(vertices, i);
  }

  distances[source] = 0;
  var verticesSize = nodes;

  for verticesSize > 0 {
    var closest int;
    for i := 0; i < nodes; i++ {
      if vertices[i] < 0 {
        continue;
      } else {
        closest = i;
        break;
      }
    }

    for i := 0; i < nodes; i++ {
      if vertices[i] < 0 {
        continue;
      }
      if distances[i] < distances[closest] {
        closest = i;
      } 
    } 
    vertices = setMinusOne(vertices, nodes, closest);
    verticesSize--;

    for i := 0; i < nodes; i++ {
      if graph[i][closest] < 0 {
        continue;
      } else if vertices[i] < 0 {
        continue;
      }

      temp := distances[closest] + graph[i][closest];
      if temp < distances[i] {
        distances[i] = temp;
      }
    }
  }

  for i := 0; i < nodes; i++ {
    if distances[i] == max {
      distances[i] = -1;
    }
  }

  return distances;
}

func addEdge(vertex1, vertex2, length int, graph [][]int) {

  graph[vertex1][vertex2] = length;
  graph[vertex2][vertex1] = length;

  return;
}

// Test the algorithm to see if it works with a sample graph
func exampleTest() {
  var myGraph [][]int;
  var nodes = 10;

  for i := 0; i < nodes; i++ {
    var row []int;
    for j := 0; j < nodes; j++ {
      row = append(row, -1);
    }
    myGraph = append(myGraph, row);
  }

  addEdge(0, 1, 8, myGraph);
  addEdge(0, 7, 6, myGraph);
  addEdge(1, 2, 5, myGraph);
  addEdge(1, 8, 7, myGraph);
  addEdge(2, 3, 6, myGraph);
  addEdge(2, 9, 5, myGraph);
  addEdge(3, 4, 5, myGraph);
  addEdge(4, 5, 8, myGraph);
  addEdge(4, 9, 4, myGraph);
  addEdge(5, 6, 4, myGraph);
  addEdge(5, 7, 6, myGraph);
  addEdge(5, 9, 3, myGraph);
  addEdge(6, 7, 4, myGraph);
  addEdge(7, 8, 4, myGraph);
  addEdge(8, 9, 3, myGraph);
  addEdge(8, 10, 10, myGraph);
  addEdge(8, 11, 9, myGraph);
  addEdge(9, 10, 11, myGraph);
  addEdge(9, 13, 3, myGraph);
  addEdge(9, 14, 7, myGraph);
  addEdge(10, 3, 20, myGraph);
  addEdge(10, 2, 15, myGraph);
  addEdge(10, 5, 99, myGraph);
  addEdge(10, 9, 11, myGraph);
  addEdge(10, 7, 14, myGraph);
  addEdge(11, 10, 21, myGraph);
  addEdge(11, 8, 99, myGraph);
  addEdge(11, 3, 12, myGraph);
  addEdge(11, 9, 17, myGraph);
  addEdge(11, 2, 33, myGraph);
  addEdge(12, 5, 21, myGraph);
  addEdge(12, 11, 69, myGraph);
  addEdge(13, 11, 20, myGraph);
  addEdge(14, 12, 18, myGraph);
  addEdge(15, 14, 7, myGraph);
  addEdge(15, 8, 19, myGraph);
  addEdge(15, 9, 5, myGraph);

  for i := 0; i < nodes; i++ {
    distances := dijkstra(myGraph, nodes, i)
    for j := 0; j < nodes; j++ {
      println ("The shortest distance from", i, "to", j, 
                "is", distances[j]);
   }   
  }

  return;
}
