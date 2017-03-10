// clause expr doesnt type check
package main

func foo() {
	var x int
	switch x=100; 100 {
	case 1.234: print(100)
	default: print(100)
	}
	return
}

