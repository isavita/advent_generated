import "io" for File

var w = {}
var gates = []

for (line in File.read("input.txt").split("\n")) {
    var s = line.trim()
    if (s == "") continue
    if (s.contains(":")) {
        var parts = s.split(":")
        w[parts[0].trim()] = Num.fromString(parts[1].trim())
    } else if (s.contains("->")) {
        var parts = s.split("->")
        var lhs = parts[0].trim().split(" ")
        gates.add({
            "i1": lhs[0],
            "op": lhs[1],
            "i2": lhs[2],
            "ot": parts[1].trim(),
            "done": false
        })
    }
}

var changed = true
while (changed) {
    changed = false
    for (g in gates) {
        if (!g["done"] && w.containsKey(g["i1"]) && w.containsKey(g["i2"])) {
            var a = w[g["i1"]]
            var b = w[g["i2"]]
            var op = g["op"]
            w[g["ot"]] = (op == "AND") ? (a & b) : (op == "OR" ? (a | b) : (a ^ b))
            g["done"] = true
            changed = true
        }
    }
}

var r = 0
for (k in w.keys) {
    if (k.startsWith("z") && w[k] == 1) {
        var bit = Num.fromString(k[1...k.count])
        var p = 1
        for (i in 0...bit) p = p * 2
        r = r + p
    }
}

System.print(r)