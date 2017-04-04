// Slices aren't comparable, thus structs containing slices
// aren't comparable, thus structs containing structs containing
// slices aren't comparable
package func_dclr

type pt struct {
	x, y, z, h string
	a int
	f struct {
		c, v []int
	}
}

type a pt

func f_slice() a {
  var x a
  x.x = "my"
  x.y = "name"
  x.z = "is"
  x.h = "tim"

  var n []int
  n = append(n, 6)
  x.f.v = n

  var y a
  y.x = "hi"

  if (x == y) {
	return y
  }
  return x
}
