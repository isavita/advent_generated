
import "io" for File

var content = File.read("input.txt")
var val = []
for (line in content.split("\n")) {
    var trimmed = line.trim()
    if (trimmed.bytes.count > 0) {
        val.add(Num.fromString(trimmed))
    }
}

var size = val.count
if (size > 0) {
    var indices = (0...size).toList
    var n = size - 1

    for (i in 0...size) {
        var v = val[i]
        var idx = indices.indexOf(i)
        indices.removeAt(idx)
        var new_idx = (idx + v) % n
        if (new_idx < 0) {
            new_idx = new_idx + n
        }
        indices.insert(new_idx, i)
    }

    var zero_idx = val.indexOf(0)
    var zero_pos = indices.indexOf(zero_idx)

    var sum = val[indices[(zero_pos + 1000) % size]] +
              val[indices[(zero_pos + 2000) % size]] +
              val[indices[(zero_pos + 3000) % size]]

    System.print(sum)
}
