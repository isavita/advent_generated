import "io" for File

var main = Fn.new {
  var input = File.read("input.txt").trim().split("\n")
  var genAStart = Num.fromString(input[0])
  var genBStart = Num.fromString(input[1])

  var genAFactor = 16807
  var genBFactor = 48271
  var modulus = 2147483647

  var genA = genAStart
  var genB = genBStart
  var matches = 0

  for (i in 0..4999999) {
    while (true) {
      genA = (genA * genAFactor) % modulus
      if (genA % 4 == 0) break
    }

    while (true) {
      genB = (genB * genBFactor) % modulus
      if (genB % 8 == 0) break
    }

    if ((genA & 0xFFFF) == (genB & 0xFFFF)) matches = matches + 1
  }

  System.print(matches)
}

main.call()