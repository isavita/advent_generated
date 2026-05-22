
import "io" for File

var orbits = {}
var content = File.read("input.txt")
for (line in content.split("\n")) {
    var trimmed = line.trim()
    if (trimmed.count > 0) {
        var parts = trimmed.split(")")
        orbits[parts[1]] = parts[0]
    }
}

var getPath = Fn.new { |start|
    var path = []
    var curr = start
    while (orbits.containsKey(curr)) {
        curr = orbits[curr]
        path.add(curr)
    }
    return path
}

var path1 = getPath.call("YOU")
var path2 = getPath.call("SAN")

var i = path1.count - 1
var j = path2.count - 1
while (i >= 0 && j >= 0 && path1[i] == path2[j]) {
    i = i - 1
    j = j - 1
}
System.print(i + j + 2)
