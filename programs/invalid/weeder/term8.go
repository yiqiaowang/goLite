//switch stmt, default is not terminating
package main


func foo () int{
	switch x:=0;{
	case true:
		return 10
	default:
	}
}
