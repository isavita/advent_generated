
import Foundation

struct Program {
    
    static func run() {
        guard let input = try? String(contentsOfFile: "input.txt", encoding: .utf8) else {
            print("Could not read input.txt")
            return
        }
        
        let rules = parseRules(input)
        let iterationsPart1 = 5
        let iterationsPart2 = 18
        
        let resultPart1 = solve(rules: rules, iterations: iterationsPart1)
        print("Pixels on after \(iterationsPart1) iterations: \(resultPart1)")

        let resultPart2 = solve(rules: rules, iterations: iterationsPart2)
        print("Pixels on after \(iterationsPart2) iterations: \(resultPart2)")
    }
    
    static func parseRules(_ input: String) -> [String: String] {
        var rules: [String: String] = [:]
        input.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: "\n").forEach { line in
            let parts = line.components(separatedBy: " => ")
            let inputPattern = parts[0]
            let outputPattern = parts[1]
            
            for variant in generateVariants(inputPattern) {
                rules[variant] = outputPattern
            }
        }
        return rules
    }
    
    static func generateVariants(_ pattern: String) -> [String] {
        var variants: [String] = []
        var currentPattern = pattern.components(separatedBy: "/")
        
        for _ in 0..<4 {
            variants.append(currentPattern.joined(separator: "/"))
            variants.append(flip(currentPattern).joined(separator: "/"))
            currentPattern = rotate(currentPattern)
        }
        return Array(Set(variants))
    }
    
    static func rotate(_ pattern: [String]) -> [String] {
        let size = pattern.count
        var rotated: [[Character]] = Array(repeating: Array(repeating: " ", count: size), count: size)
        
        for i in 0..<size {
            for j in 0..<size {
                rotated[j][size - 1 - i] = pattern[i][pattern[i].index(pattern[i].startIndex, offsetBy: j)]
            }
        }
        
        return rotated.map { String($0) }
    }
    
    static func flip(_ pattern: [String]) -> [String] {
        return pattern.map { String($0.reversed()) }
    }
    
    static func solve(rules: [String: String], iterations: Int) -> Int {
        var grid = [".#.", "..#", "###"]
        
        for _ in 0..<iterations {
            let size = grid.count
            let subgridSize = size % 2 == 0 ? 2 : 3
            let newSubgridSize = subgridSize == 2 ? 3 : 4
            let numSubgrids = size / subgridSize
            var newGrid: [[String]] = Array(repeating: Array(repeating: "", count: numSubgrids), count: numSubgrids)

            for i in 0..<numSubgrids {
                for j in 0..<numSubgrids {
                    var subgrid: [String] = []
                    for row in (i * subgridSize)..<(i * subgridSize + subgridSize) {
                        let start = grid[row].index(grid[row].startIndex, offsetBy: j * subgridSize)
                        let end = grid[row].index(start, offsetBy: subgridSize)
                        subgrid.append(String(grid[row][start..<end]))
                    }
                    
                    let subgridKey = subgrid.joined(separator: "/")
                    newGrid[i][j] = rules[subgridKey]!
                }
            }
          
            var combinedGrid: [String] = []
            for i in 0..<numSubgrids{
                for row in 0..<newSubgridSize{
                    var combinedRow = ""
                    for j in 0..<numSubgrids {
                        let parts = newGrid[i][j].components(separatedBy: "/")
                        combinedRow += parts[row]
                    }
                    combinedGrid.append(combinedRow)
                }
            }
            grid = combinedGrid
        }

        return grid.reduce(0) { count, row in
            count + row.filter { $0 == "#" }.count
        }
    }
}
Program.run()
