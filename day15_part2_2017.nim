
import os
import strutils

var file = open("input.txt")
let genAStart = parseInt(file.readLine())
let genBStart = parseInt(file.readLine())

let genAFactor = 16807
let genBFactor = 48271
let modulus = 2147483647

var genA = genAStart
var genB = genBStart
var matches = 0

for i in 0..<5000000:
    while true:
        genA = genA * genAFactor mod modulus
        if genA mod 4 == 0:
            break

    while true:
        genB = genB * genBFactor mod modulus
        if genB mod 8 == 0:
            break

    if (genA and 0xFFFF) == (genB and 0xFFFF):
        matches.inc()

echo matches
