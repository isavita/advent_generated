import Foundation

func main() {
    do {
        let fileContent = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let regex = try! NSRegularExpression(pattern: "Game (\\d+): (.+)", options: [])
        let cubeRegex = try! NSRegularExpression(pattern: "(\\d+) (red|green|blue)", options: [])
        var totalPower = 0

        let lines = fileContent.components(separatedBy: "\n")
        for line in lines {
            let matches = regex.matches(in: line, options: [], range: NSRange(location: 0, length: line.utf16.count))
            if matches.count > 0 {
                var matchesArray = matches.map { (match) -> [String] in
                    (0..<match.numberOfRanges).map { _ in "" }
                }
                for (index, match) in matches.enumerated() {
                    for i in 0..<match.numberOfRanges {
                        let range = match.range(at: i)
                        matchesArray[index][i] = (line as NSString).substring(with: range)
                    }
                }
                let rounds = matchesArray[0][2].components(separatedBy: ";")
                var maxRed = 0, maxGreen = 0, maxBlue = 0

                for round in rounds {
                    let cubes = cubeRegex.matches(in: round, options: [], range: NSRange(location: 0, length: round.utf16.count))
                    var red = 0, green = 0, blue = 0
                    for cube in cubes {
                        let count = Int((round as NSString).substring(with: cube.range(at: 1)))!
                        switch (round as NSString).substring(with: cube.range(at: 2)) {
                        case "red":
                            red += count
                        case "green":
                            green += count
                        case "blue":
                            blue += count
                        default:
                            break
                        }
                    }
                    if red > maxRed {
                        maxRed = red
                    }
                    if green > maxGreen {
                        maxGreen = green
                    }
                    if blue > maxBlue {
                        maxBlue = blue
                    }
                }
                let power = maxRed * maxGreen * maxBlue
                totalPower += power
            }
        }
        print(totalPower)
    } catch {
        print("Error reading file:", error)
    }
}

main()