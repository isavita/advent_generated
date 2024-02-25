
import os
import strutils
import tables

var passphrases = readFile("input.txt").splitLines()
var validCount = 0

for passphrase in passphrases:
    var words = passphrase.split()
    var wordSet = initTable[string, bool]()

    var valid = true
    for word in words:
        if word in wordSet:
            valid = false
            break
        wordSet[word] = true

    if valid:
        validCount.inc()

echo validCount
