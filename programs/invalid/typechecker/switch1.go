// init doesnt type check
package main

func foo() {
	switch x:=y; 100{
	case 100:
		print(100)
	default: print(100)
	}
	return
}

