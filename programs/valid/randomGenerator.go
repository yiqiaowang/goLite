// A completely customizable Random Number Generator
package random;

var (
  seed int = 0
  multiplier int = 23961
  adder int = 53785935
  modder int = 38199; )


// get and set the seed
func setSeed(nSeed int) {
  seed = nSeed;
  return;
}

func getSeed() int {
  return seed;
}

// get and set the multiplying number used in the random number generator
func setMultiplier(nMultiplier int) {
  multiplier = nMultiplier;
  return;
}

func getMultiplier() int {
  return multiplier;
}

// get and set the adding number used in the random number generator
func setAdder(nAdder int) {
  adder = nAdder;
  return;
}

func getAdder() int {
  return adder;
}

// get and set the modding number used in the random number generator
func setModder(nModder int) {
  modder = nModder;
  return;
}

func getModder() int {
  return modder;
}

// Get the next random int between min and max (min included)
func getNextInt(min, max int) int {
  mod := max - min;

  seed = (seed * multiplier + adder) % modder;

  return seed % mod + min;
}

// Test the algorithm to see if it works
func main() {
  var mySeed = 83752;

  setSeed(mySeed);

  for i := 0; i < 100; i++ {
    println ("next random number between 0 and 10 is",
                getNextInt(0, 10))
  }

  return;
}
