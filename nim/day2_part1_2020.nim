
import strutils

var inputFile = open("input.txt")
var validPasswords = 0

for line in inputFile.lines:
    var parts = line.split(" ")
    var range = parts[0].split("-")
    var minCount = parseInt(range[0])
    var maxCount = parseInt(range[1])
    var targetChar = parts[1][0]
    var password = parts[2]

    var count = password.count(targetChar)

    if count >= minCount and count <= maxCount:
        validPasswords += 1

inputFile.close()

echo validPasswords
