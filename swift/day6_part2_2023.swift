
import Foundation

func calculateWaysToWinLongRace(time: Int, record: Int) -> Int {
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

func solve() throws -> Int {
    let fileURL = URL(fileURLWithPath: "input.txt")
    let input = try String(contentsOf: fileURL, encoding: .utf8)
    let lines = input.components(separatedBy: .newlines).filter { !$0.isEmpty }

    let time = Int(lines[0].components(separatedBy: ":")[1].replacingOccurrences(of: " ", with: ""))!
    let distance = Int(lines[1].components(separatedBy: ":")[1].replacingOccurrences(of: " ", with: ""))!

    let waysToWin = calculateWaysToWinLongRace(time: time, record: distance)
    return waysToWin
}

do {
    let result = try solve()
    print(result)
} catch {
    print("Error: \(error)")
}
