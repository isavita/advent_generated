
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
guard let fileContents = try? String(contentsOf: fileURL) else { exit(1) }

let regex = try! NSRegularExpression(pattern: "Game (\\d+): (.+)")
let cubeRegex = try! NSRegularExpression(pattern: "(\\d+) (red|green|blue)")
var totalSum = 0

fileContents.enumerateLines { line, _ in
    let matches = regex.matches(in: line, range: NSRange(location: 0, length: line.utf16.count))
    
    if let match = matches.first, match.numberOfRanges == 3 {
        let gameId = Int((line as NSString).substring(with: match.range(at: 1)))!
        let rounds = (line as NSString).substring(with: match.range(at: 2)).split(separator: ";")
        var isValid = true
        
        for round in rounds {
            let cubes = cubeRegex.matches(in: String(round), range: NSRange(location: 0, length: round.utf16.count))
            var red = 0, green = 0, blue = 0
            
            for cube in cubes {
                let count = Int((String(round) as NSString).substring(with: cube.range(at: 1)))!
                let color = (String(round) as NSString).substring(with: cube.range(at: 2))
                
                switch color {
                case "red": red += count
                case "green": green += count
                case "blue": blue += count
                default: break
                }
                
                if red > 12 || green > 13 || blue > 14 {
                    isValid = false
                    break
                }
            }
            
            if !isValid { break }
        }
        
        if isValid { totalSum += gameId }
    }
}

print(totalSum)
