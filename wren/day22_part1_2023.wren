
import "io" for File

class Brick {
    construct new(x1, y1, z1, x2, y2, z2) {
        _x1 = x1
        _y1 = y1
        _z1 = z1
        _x2 = x2
        _y2 = y2
        _z2 = z2
    }
    x1 { _x1 }
    y1 { _y1 }
    z1 { _z1 }
    x2 { _x2 }
    y2 { _y2 }
    z2 { _z2 }
    z1=(v) { _z1 = v }
    z2=(v) { _z2 = v }
}

class QuickSort {
    static sort(list) {
        sort(list, 0, list.count - 1)
    }
    static sort(list, low, high) {
        if (low < high) {
            var p = partition(list, low, high)
            sort(list, low, p - 1)
            sort(list, p + 1, high)
        }
    }
    static partition(list, low, high) {
        var pivot = list[high].z1
        var i = low - 1
        for (j in low...high) {
            if (list[j].z1 <= pivot) {
                i = i + 1
                var temp = list[i]
                list[i] = list[j]
                list[j] = temp
            }
        }
        var temp = list[i + 1]
        list[i + 1] = list[high]
        list[high] = temp
        return i + 1
    }
}

var lines = File.read("input.txt").trim().split("\n")
var bricks = []
for (line in lines) {
    var trimmed = line.trim()
    if (trimmed == "") continue
    var parts = trimmed.split("~")
    var p1 = parts[0].split(",").map { |x| Num.fromString(x) }.toList
    var p2 = parts[1].split(",").map { |x| Num.fromString(x) }.toList
    for (i in 0..2) {
        if (p1[i] > p2[i]) {
            var temp = p1[i]
            p1[i] = p2[i]
            p2[i] = temp
        }
    }
    bricks.add(Brick.new(p1[0], p1[1], p1[2], p2[0], p2[1], p2[2]))
}

QuickSort.sort(bricks)

var maxX = 0
var maxY = 0
for (b in bricks) {
    if (b.x2 > maxX) maxX = b.x2
    if (b.y2 > maxY) maxY = b.y2
}

var h = List.filled(maxX + 1, null)
var top = List.filled(maxX + 1, null)
for (i in 0..maxX) {
    h[i] = List.filled(maxY + 1, 0)
    top[i] = List.filled(maxY + 1, 0)
}

var n = bricks.count
var supps = List.filled(n + 1, null)
for (i in 0..n) supps[i] = []
var nb = List.filled(n + 1, 0)

for (i in 0...n) {
    var b = bricks[i]
    var id = i + 1
    var mh = 0
    for (x in b.x1..b.x2) {
        for (y in b.y1..b.y2) {
            if (h[x][y] > mh) mh = h[x][y]
        }
    }
    var seen = {}
    for (x in b.x1..b.x2) {
        for (y in b.y1..b.y2) {
            if (h[x][y] == mh && top[x][y] > 0) {
                var s = top[x][y]
                if (!seen.containsKey(s)) {
                    seen[s] = true
                    supps[s].add(id)
                    nb[id] = nb[id] + 1
                }
            }
        }
    }
    var dz = b.z2 - b.z1
    b.z1 = mh + 1
    b.z2 = b.z1 + dz
    for (x in b.x1..b.x2) {
        for (y in b.y1..b.y2) {
            h[x][y] = b.z2
            top[x][y] = id
        }
    }
}

var ans = 0
for (i in 1..n) {
    var safe = true
    for (s in supps[i]) {
        if (nb[s] == 1) {
            safe = false
            break
        }
    }
    if (safe) ans = ans + 1
}

System.print(ans)
