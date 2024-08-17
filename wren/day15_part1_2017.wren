import "io" for File

var input = File.read("input.txt").split("\n")
var genAStart = Num.fromString(input[0])
var genBStart = Num.fromString(input[1])
var genAFactor = 16807
var genBFactor = 48271
var modulus = 2147483647
var genA = genAStart
var genB = genBStart
var matches = 0

for (i in 0..40000000) {
    genA = (genA * genAFactor) % modulus
    genB = (genB * genBFactor) % modulus
    if ((genA & 0xFFFF) == (genB & 0xFFFF)) {
        matches = matches + 1
    }
}

System.print(matches)