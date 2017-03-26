/*
* List struct that maintains a length field. In GoLite, there's no way
* of querying the length of a slice, nor is there a way of removing
* values from a slice. Due to this limitation, building data structures
* (like a hash table) that rely on dynamically changing lists is difficult.
* This list provides a both lenght and reomval operations on lists.
*
* Running the main function takes approximately .4s on the canonical Go
* compiler.
*/
package main

//List abstraction b/c GoLite doesnt have len/cap builtins
type List struct {
  len int
  contents []int
}

//used to represent failed operations
var NIL_LIST List

//
func length(l List) int {
  return l.len
}

//
func validIndex(l List, index int) bool {
  return index < l.len && index >= 0
}

//
func get(l List, index int) int {
  if validIndex(l, index) {
    return l.contents[index]
  } else {
    return -1
  }
}

// remove element by
func remove(l List, index int) List {
  if validIndex(l, index) {
    //replace old contents with new slice without value at index
    var newContents []int
    for i := 0; i < l.len; i++ {
      if i != index {
        newContents = append(newContents, l.contents[i])
      }
    }
    l.contents = newContents
    l.len --

    return l
  } else {
    return NIL_LIST
  }
}

func push(l List, value int) List {
  l.contents = append(l.contents, value)
  l.len ++
  return l
}

//Benchmark the list by inserting 10K elements into the list.
func main() {
  var l List

  //push 10K values
  for i := 0; i < 10000; i++ {
    l = push(l, i)
  }

  //remove all the values
  for i := 0; i < 10000; i++ {
    l = remove(l, 0)
  }
}
