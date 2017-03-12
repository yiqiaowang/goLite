package func_dclr

type pt struct {
	x, y, z, h string
	a int
	f struct {
		c, v [5]int
	}
}

type a pt


func f_slice() a {
  var x a
  x.x = "my"
  x.y = "name"
  x.z = "is"
  x.h = "tim"

  var n [5]int
  n[0] = 6

  x.f.v = n

  var y a
  y.x = "hi"

  if (x == y) {
  	return y
  }
  return x
}
