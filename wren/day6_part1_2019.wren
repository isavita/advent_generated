
import "io" for File

var content = File.read("input.txt").replace("\r", "")
var orbits = {}
for (line in content.split("\n")) {
    if (line != "") {
        var parts = line.split(")")
        orbits[parts[1]] = parts[0]
    }
}

var sum = 0
for (obj in orbits.keys) {
    var curr = obj
    while (orbits.containsKey(curr)) {
        sum = sum + 1
        curr = orbits[curr]
    }
}
System.print(sum)
