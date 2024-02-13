
import Foundation

let input = try String(contentsOfFile: "input.txt")
let tiles = input.components(separatedBy: "\n\n")

var tileMap: [Int: [[Character]]] = [:]

for tile in tiles {
    let lines = tile.components(separatedBy: .newlines)
    let id = Int(lines[0].replacingOccurrences(of: "Tile ", with: "").replacingOccurrences(of: ":", with: ""))!
    let image = lines[1...].map { Array($0) }
    tileMap[id] = image
}

var cornerProduct = 1

for (id, image) in tileMap {
    var count = 0
    let borders = getBorders(image)
    
    for border in borders {
        if let matchingTileID = findMatchingTileID(tileMap: tileMap, border: border, currentTileID: id) {
            count += 1
        }
    }
    
    if count == 2 {
        cornerProduct *= id
    }
}

print(cornerProduct)

func getBorders(_ image: [[Character]]) -> [String] {
    let top = String(image[0])
    let bottom = String(image[image.count - 1])
    let left = String(image.map { $0[0] })
    let right = String(image.map { $0[image.count - 1] })
    
    return [top, bottom, left, right]
}

func findMatchingTileID(tileMap: [Int: [[Character]]], border: String, currentTileID: Int) -> Int? {
    for (id, image) in tileMap {
        if id != currentTileID {
            let borders = getBorders(image)
            if borders.contains(border) || borders.contains(String(border.reversed())) {
                return id
            }
        }
    }
    
    return nil
}
