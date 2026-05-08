
import "io" for File

var content = File.read("input.txt")
var lines = content.split("\n")
var validPasswords = 0

for (line in lines) {
    if (line.isEmpty) continue
    var parts = line.split(" ")
    var rangeParts = parts[0].split("-")
    var minCount = Num.fromString(rangeParts[0])
    var maxCount = Num.fromString(rangeParts[1])
    var targetChar = parts[1][0]
    var password = parts[2]

    var count = 0
    for (c in password) {
        if (c == targetChar) count = count + 1
    }

    if (count >= minCount && count <= maxCount) {
        validPasswords = validPasswords + 1
    }
}

System.print(validPasswords)
