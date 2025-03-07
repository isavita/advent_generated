
import Foundation

struct Tile {
    let id: Int
    var contents: [[Character]]
}

func parseTiles(from input: String) -> [Tile] {
    let blocks = input.components(separatedBy: "\n\n")
    return blocks.map { block in
        let lines = block.components(separatedBy: "\n")
        let tileId = Int(lines[0].components(separatedBy: " ")[1].dropLast())!
        let contents = lines.dropFirst().map { Array($0) }
        return Tile(id: tileId, contents: contents)
    }
}

func getCol(grid: [[Character]], firstCol: Bool) -> String {
    return grid.map { String($0[firstCol ? 0 : $0.count - 1]) }.joined()
}

func getRow(grid: [[Character]], firstRow: Bool) -> String {
    return String(firstRow ? grid[0] : grid[grid.count - 1])
}

func removeBorders(from grid: [[Character]]) -> [[Character]] {
    guard grid.count > 2 && grid[0].count > 2 else { return [] }
    return Array(grid[1..<grid.count - 1].map { Array($0[1..<$0.count - 1]) })
}

func rotateGrid(grid: [[Character]]) -> [[Character]] {
    guard !grid.isEmpty else { return [] }
    let rows = grid.count
    let cols = grid[0].count
    var newGrid = Array(repeating: Array(repeating: Character(" "), count: rows), count: cols)

    for i in 0..<rows {
        for j in 0..<cols {
            newGrid[j][rows - 1 - i] = grid[i][j]
        }
    }
    return newGrid
}

func mirrorGrid(grid: [[Character]]) -> [[Character]] {
    return grid.map { $0.reversed() }
}

func allOrientations(grid: [[Character]]) -> [[[Character]]] {
    var orientations: [[[Character]]] = [grid]
    for _ in 0..<3 {
        orientations.append(rotateGrid(grid: orientations.last!))
    }
    for i in 0..<4 {
        orientations.append(mirrorGrid(grid: orientations[i]))
    }
    return orientations
}

func backtrackAssemble(tiles: [Tile], assembledTiles: inout [[Tile?]], usedIndices: inout Set<Int>) -> Bool {
    let edgeSize = Int(sqrt(Double(tiles.count)))
    
    for row in 0..<edgeSize {
        for col in 0..<edgeSize {
            if assembledTiles[row][col] == nil {
                for i in 0..<tiles.count {
                    if !usedIndices.contains(i) {
                        for orientation in allOrientations(grid: tiles[i].contents) {
                            var currentTile = tiles[i]
                            currentTile.contents = orientation
                            
                            if row != 0 {
                                let currentTopRow = getRow(grid: currentTile.contents, firstRow: true)
                                let bottomOfAbove = getRow(grid: assembledTiles[row-1][col]!.contents, firstRow: false)
                                if currentTopRow != bottomOfAbove {
                                    continue
                                }
                            }
                            if col != 0 {
                                let currentLeftCol = getCol(grid: currentTile.contents, firstCol: true)
                                let rightColOfLeft = getCol(grid: assembledTiles[row][col-1]!.contents, firstCol: false)
                                if currentLeftCol != rightColOfLeft {
                                    continue
                                }
                            }
                            
                            assembledTiles[row][col] = currentTile
                            usedIndices.insert(i)
                            
                            if backtrackAssemble(tiles: tiles, assembledTiles: &assembledTiles, usedIndices: &usedIndices) {
                                return true
                            }
                            
                            assembledTiles[row][col] = nil
                            usedIndices.remove(i)
                        }
                    }
                }
                return false
            }
        }
    }
    return true
}

func findMonsterCoords(image: [[Character]]) -> [(Int, Int)] {
    let monster = [
        "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   ",
    ]
    let monsterOffsets = monster.enumerated().flatMap { r, line in
        line.enumerated().compactMap { c, char in
            char == "#" ? (r, c) : nil
        }
    }
    let monsterHeight = monster.count
    let monsterLength = monster[0].count
    
    var monsterStartingCoords: [(Int, Int)] = []
    for r in 0...(image.count - monsterHeight) {
        for c in 0...(image[0].count - monsterLength) {
            var monsterFound = true
            for (dr, dc) in monsterOffsets {
                if image[r+dr][c+dc] != "#" {
                    monsterFound = false
                    break
                }
            }
            if monsterFound {
                monsterStartingCoords.append((r, c))
            }
        }
    }
    
    var monsterCoords: [(Int, Int)] = []
    for (r, c) in monsterStartingCoords {
        for (dr, dc) in monsterOffsets {
            monsterCoords.append((r+dr, c+dc))
        }
    }
    
    return monsterCoords
}

func solve(input: String) -> Int {
    let tiles = parseTiles(from: input)
    let edgeSize = Int(sqrt(Double(tiles.count)))
    
    var assembledTiles: [[Tile?]] = Array(repeating: Array(repeating: nil, count: edgeSize), count: edgeSize)
    var usedIndices: Set<Int> = []
    
    guard backtrackAssemble(tiles: tiles, assembledTiles: &assembledTiles, usedIndices: &usedIndices) else {
        return 0
    }
    
    var coreImage: [[Character]] = []
    for bigRow in 0..<edgeSize {
        for subRow in 0..<removeBorders(from: assembledTiles[0][0]!.contents).count {
            var row: [Character] = []
            for bigCol in 0..<edgeSize {
                let tileContent = removeBorders(from: assembledTiles[bigRow][bigCol]!.contents)
                row.append(contentsOf: tileContent[subRow])
            }
            coreImage.append(row)
        }
    }

    var image = coreImage
    var monsterCoords: [(Int, Int)] = []
    for orientation in allOrientations(grid: coreImage) {
        let foundCoords = findMonsterCoords(image: orientation)
        if !foundCoords.isEmpty {
            image = orientation
            monsterCoords = foundCoords
            break
        }
    }

    var markedImage = image
    for (r, c) in monsterCoords {
        markedImage[r][c] = "O"
    }

    let roughWatersCount = markedImage.flatMap { $0 }.filter { $0 == "#" }.count
    return roughWatersCount
}

if let input = try? String(contentsOfFile: "input.txt", encoding: .utf8) {
    let result = solve(input: input.trimmingCharacters(in: .whitespacesAndNewlines))
    print(result)
} else {
    print("Error reading input file")
}
