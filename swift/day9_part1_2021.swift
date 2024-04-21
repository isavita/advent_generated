import Foundation

func main() {
    do {
        let fileContent = try String(contentsOfFile: "input.txt")
        let heightmap = fileContent.components(separatedBy: "\n").map { line in
            line.compactMap { Int(String($0)) }
        }
        
        var totalRiskLevel = 0
        for y in 0..<heightmap.count {
            for x in 0..<heightmap[y].count {
                if isLowPoint(heightmap, x: x, y: y) {
                    totalRiskLevel += 1 + heightmap[y][x]
                }
            }
        }
        
        print(totalRiskLevel)
    } catch {
        print("Error reading file: \(error)")
    }
}

func isLowPoint(_ heightmap: [[Int]], x: Int, y: Int) -> Bool {
    let height = heightmap[y][x]
    if x > 0 && heightmap[y][x-1] <= height { return false }
    if x < heightmap[y].count-1 && heightmap[y][x+1] <= height { return false }
    if y > 0 && heightmap[y-1][x] <= height { return false }
    if y < heightmap.count-1 && heightmap[y+1][x] <= height { return false }
    return true
}

main()