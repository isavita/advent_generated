
import "io" for File

var content = File.read("input.txt")
var lines = content.split("\n")
var claims = []

for (line in lines) {
    var trimmed = line.trim()
    if (trimmed == "") continue
    var parts = trimmed.split(" ")
    if (parts.count < 4) continue
    var id = Num.fromString(parts[0][1..-1])
    var coords = parts[2][0..-2].split(",")
    var x = Num.fromString(coords[0])
    var y = Num.fromString(coords[1])
    var dims = parts[3].split("x")
    var w = Num.fromString(dims[0])
    var h = Num.fromString(dims[1])
    claims.add([id, x, y, w, h])
}

var fabric = List.filled(1000000, 0)
for (claim in claims) {
    var x = claim[1]
    var y = claim[2]
    var w = claim[3]
    var h = claim[4]
    for (i in y...(y + h)) {
        var rowOffset = i * 1000
        for (j in x...(x + w)) {
            var idx = rowOffset + j
            fabric[idx] = fabric[idx] + 1
        }
    }
}

for (claim in claims) {
    var id = claim[0]
    var x = claim[1]
    var y = claim[2]
    var w = claim[3]
    var h = claim[4]
    var overlap = false
    for (i in y...(y + h)) {
        var rowOffset = i * 1000
        for (j in x...(x + w)) {
            if (fabric[rowOffset + j] > 1) {
                overlap = true
                break
            }
        }
        if (overlap) break
    }
    if (!overlap) {
        System.print(id)
        break
    }
}
