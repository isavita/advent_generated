import "io" for File

var content = File.read("input.txt")
var leftList = []
var rightList = []

for (line in content.split("\n")) {
    if (line.isEmpty) continue
    var line2 = line.replace("\t", " ")
    var parts = []
    for (word in line2.split(" ")) {
        if (word != "") parts.add(word)
    }
    if (parts.count >= 2) {
        leftList.add(Num.fromString(parts[0]))
        rightList.add(Num.fromString(parts[-1]))
    }
}

leftList.sort()
rightList.sort()

var total = 0
for (i in 0...leftList.count) {
    total = total + (leftList[i] - rightList[i]).abs
}

System.print("Total distance: %(total)")