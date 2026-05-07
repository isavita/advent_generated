
import "io" for File

var main = Fn.new {
    var inputData = File.read("input.txt").trim()
    var lines = inputData.split("\n")
    var parts = []
    var partsGrid = {}
    var gears = []
    var parsingNumber = false
    var currentPart = null
    var addPart = Fn.new { |part|
        var idx = parts.count
        parts.add(part)
        var y = part["y"]
        for (x in part["xmin"]..part["xmax"]) {
            var key = "%(x),%(y)"
            partsGrid[key] = idx
        }
    }
    for (y in 0...lines.count) {
        var line = lines[y]
        if (parsingNumber) {
            addPart.call(currentPart)
            parsingNumber = false
        }
        var bytes = line.bytes
        for (x in 0...bytes.count) {
            var b = bytes[x]
            if (b >= 48 && b <= 57) {
                var digit = b - 48
                if (!parsingNumber) {
                    currentPart = {"xmin": x, "xmax": x, "y": y, "n": digit}
                    parsingNumber = true
                } else {
                    currentPart["n"] = currentPart["n"] * 10 + digit
                    currentPart["xmax"] = x
                }
            } else {
                if (parsingNumber) {
                    addPart.call(currentPart)
                    parsingNumber = false
                }
                if (b == 42) {
                    gears.add([x, y])
                }
            }
        }
    }
    if (parsingNumber) {
        addPart.call(currentPart)
    }
    var sumVal = 0
    var neighbors = [[0,1], [0,-1], [1,0], [-1,0], [-1,-1], [-1,1], [1,-1], [1,1]]
    for (gear in gears) {
        var gx = gear[0]
        var gy = gear[1]
        var neighborParts = {}
        for (offset in neighbors) {
            var nx = gx + offset[0]
            var ny = gy + offset[1]
            var key = "%(nx),%(ny)"
            var val = partsGrid[key]
            if (val != null) {
                neighborParts[val] = true
            }
        }
        var indices = neighborParts.keys
        if (indices.count == 2) {
            var prod = 1
            for (i in indices) {
                prod = prod * parts[i]["n"]
            }
            sumVal = sumVal + prod
        }
    }
    System.print(sumVal)
}
main.call()
