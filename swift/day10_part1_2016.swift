
import Foundation

struct Bot {
    var lowTo: String
    var highTo: String
    var chips: [Int]
}

var bots = [String: Bot]()
let fileURL = URL(fileURLWithPath: "input.txt")
let fileContents = try String(contentsOf: fileURL)
let lines = fileContents.components(separatedBy: .newlines)

let valueRegex = try! NSRegularExpression(pattern: "value (\\d+) goes to (bot \\d+)")
let givesRegex = try! NSRegularExpression(pattern: "(bot \\d+) gives low to (bot \\d+|output \\d+) and high to (bot \\d+|output \\d+)")

for line in lines {
    if let valueMatch = valueRegex.firstMatch(in: line, range: NSRange(line.startIndex..., in: line)) {
        let value = Int((line as NSString).substring(with: valueMatch.range(at: 1)))!
        let botID = (line as NSString).substring(with: valueMatch.range(at: 2))

        if bots[botID] == nil {
            bots[botID] = Bot(lowTo: "", highTo: "", chips: [])
        }
        bots[botID]?.chips.append(value)
    } else if let givesMatch = givesRegex.firstMatch(in: line, range: NSRange(line.startIndex..., in: line)) {
        let botID = (line as NSString).substring(with: givesMatch.range(at: 1))
        let lowTo = (line as NSString).substring(with: givesMatch.range(at: 2))
        let highTo = (line as NSString).substring(with: givesMatch.range(at: 3))

        if bots[botID] == nil {
            bots[botID] = Bot(lowTo: "", highTo: "", chips: [])
        }
        bots[botID]?.lowTo = lowTo
        bots[botID]?.highTo = highTo
    }
}

while true {
    var action = false
    for (botID, bot) in bots {
        if bot.chips.count == 2 {
            action = true
            let (low, high) = (bot.chips.min()!, bot.chips.max()!)
            if low == 17 && high == 61 {
                print(botID)
                exit(0)
            }
            bots[botID]?.chips = []

            giveChip(target: bot.lowTo, value: low)
            giveChip(target: bot.highTo, value: high)
        }
    }
    if !action {
        break
    }
}

func giveChip(target: String, value: Int) {
    if bots[target] == nil {
        bots[target] = Bot(lowTo: "", highTo: "", chips: [])
    }
    bots[target]?.chips.append(value)
}
