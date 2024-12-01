
import Foundation

func calculateWaysToWin(time: Int, record: Int) -> Int {
    var waysToWin = 0
    for holdTime in 1..<time {
        let travelTime = time - holdTime
        let distance = holdTime * travelTime
        if distance > record {
            waysToWin += 1
        }
    }
    return waysToWin
}

func main() {
    do {
        let fileURL = URL(fileURLWithPath: "input.txt")
        let fileContents = try String(contentsOf: fileURL)
        let lines = fileContents.components(separatedBy: .newlines)
        
        guard lines.count == 2 else {
            print("Error: Input file must contain two lines.")
            return
        }

        let times = lines[0].split(separator: " ").compactMap { Int($0) }
        let distances = lines[1].split(separator: " ").compactMap { Int($0) }

        guard times.count == distances.count else {
            print("Error: Times and distances arrays must have the same length.")
            return
        }

        var totalWays = 1
        for i in 0..<times.count {
            let ways = calculateWaysToWin(time: times[i], record: distances[i])
            totalWays *= ways
        }

        print(totalWays)
    } catch {
        print("Error reading file: \(error)")
    }
}

main()
