package type_dclrs

type t1 bool
type t2 t1
type t3 t2

var x t1 = true
var y t2 = x
var z t3 = y
