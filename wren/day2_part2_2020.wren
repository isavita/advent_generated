
import "io" for File

var validCount = 0

var content = File.read("input.txt")
if (content == null) {
    System.print("Error reading file")
    return
}

content = content.replace("\r\n", "\n")
var lines = content.split("\n")
for (line in lines) {
    if (line == "") continue
    var parts = line.split(" ")
    if (parts.count < 3) continue
    var rangePart = parts[0]
    var charPart = parts[1]
    var password = parts[2]

    var rangeSplit = rangePart.split("-")
    if (rangeSplit.count < 2) continue
    var pos1 = Num.fromString(rangeSplit[0])
    var pos2 = Num.fromString(rangeSplit[1])
    if (pos1 == null || pos2 == null) continue

    var char = charPart[0]
    var idx1 = pos1 - 1
    var idx2 = pos2 - 1

    var count = 0
    if (password[idx1] == char) count = count + 1
    if (password[idx2] == char) count = count + 1

    if (count == 1) validCount = validCount + 1
}

System.print(validCount)
