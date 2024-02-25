
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

for i in 0 ..< 40000000:
    genA = (genA * genAFactor) mod modulus
    genB = (genB * genBFactor) mod modulus

    if (genA and 0xFFFF) == (genB and 0xFFFF):
        matches.inc

echo matches
