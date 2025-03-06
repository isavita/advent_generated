
import Foundation

func canFormDesign(design: String, patterns: [String]) -> Bool {
    var dp = [Bool](repeating: false, count: design.count + 1)
    dp[0] = true

    for i in 0..<design.count {
        if dp[i] {
            for pattern in patterns {
                if i + pattern.count <= design.count {
                    let startIndex = design.index(design.startIndex, offsetBy: i)
                    let endIndex = design.index(startIndex, offsetBy: pattern.count)
                    let subDesign = design[startIndex..<endIndex]
                    
                    if subDesign == pattern {
                        dp[i + pattern.count] = true
                    }
                }
            }
        }
    }

    return dp[design.count]
}

func main() {
    do {
        
        let input = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let parts = input.components(separatedBy: "\n\n")
        guard parts.count == 2 else {
            print("Invalid input format")
            return
        }

        let patterns = parts[0].components(separatedBy: ",").map { $0.trimmingCharacters(in: .whitespacesAndNewlines) }
        let designs = parts[1].components(separatedBy: .newlines).map { $0.trimmingCharacters(in: .whitespacesAndNewlines) }.filter { !$0.isEmpty }

        var possibleDesignsCount = 0
        for design in designs {
            if canFormDesign(design: design, patterns: patterns) {
                possibleDesignsCount += 1
            }
        }

        print(possibleDesignsCount)

    } catch {
        print("Error reading file: \(error)")
    }
}

main()
