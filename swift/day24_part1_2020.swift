
import Foundation

let input = try String(contentsOfFile: "input.txt")
let directions = input.components(separatedBy: "\n")

var blackTiles: Set<String> = []

for direction in directions {
    var x = 0
    var y = 0
    var z = 0
    
    var index = direction.startIndex
    
    while index < direction.endIndex {
        if direction[index] == "e" {
            x += 1
            y -= 1
            index = direction.index(after: index)
        } else if direction[index] == "w" {
            x -= 1
            y += 1
            index = direction.index(after: index)
        } else if direction[index...].hasPrefix("se") {
            y -= 1
            z += 1
            index = direction.index(index, offsetBy: 2)
        } else if direction[index...].hasPrefix("nw") {
            y += 1
            z -= 1
            index = direction.index(index, offsetBy: 2)
        } else if direction[index...].hasPrefix("sw") {
            x -= 1
            z += 1
            index = direction.index(index, offsetBy: 2)
        } else if direction[index...].hasPrefix("ne") {
            x += 1
            z -= 1
            index = direction.index(index, offsetBy: 2)
        }
    }
    
    let tile = "\(x),\(y),\(z)"
    
    if blackTiles.contains(tile) {
        blackTiles.remove(tile)
    } else {
        blackTiles.insert(tile)
    }
}

print(blackTiles.count)
