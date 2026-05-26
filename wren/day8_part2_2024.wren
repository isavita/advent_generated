import "io" for File

class Math {
    static gcd(a, b) {
        a = a.abs
        b = b.abs
        while (b != 0) {
            var t = b
            b = a % b
            a = t
        }
        return a
    }
}

var content = File.read("input.txt")
var lines = content.split("\n").map { |s| s.trim() }.where { |s| s.bytes.count > 0 }.toList
var h = lines.count
var w = lines[0].bytes.count

var antennas = {}
for (y in 0...h) {
    var line = lines[y]
    for (x in 0...w) {
        var c = line[x]
        if (c != ".") {
            if (!antennas.containsKey(c)) {
                antennas[c] = []
            }
            antennas[c].add([x, y])
        }
    }
}

var antinodes = {}
for (freq in antennas.keys) {
    var coords = antennas[freq]
    if (coords.count < 2) continue
    for (i in 0...(coords.count - 1)) {
        for (j in (i + 1)...coords.count) {
            var A = coords[i]
            var B = coords[j]
            var dx = B[0] - A[0]
            var dy = B[1] - A[1]
            var g = Math.gcd(dx, dy)
            var sx = (dx / g).truncate
            var sy = (dy / g).truncate
            
            var x = A[0]
            var y = A[1]
            while (x >= 0 && x < w && y >= 0 && y < h) {
                antinodes["%(x),%(y)"] = true
                x = x + sx
                y = y + sy
            }
            
            x = A[0] - sx
            y = A[1] - sy
            while (x >= 0 && x < w && y >= 0 && y < h) {
                antinodes["%(x),%(y)"] = true
                x = x - sx
                y = y - sy
            }
        }
    }
}

System.print(antinodes.count)