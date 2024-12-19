
import Foundation

struct Record {
    let timestamp: Date
    let action: String
    let guardID: Int
}

func readAndParseInput(filename: String) -> [Record] {
    guard let fileURL = URL(string: "file://" + FileManager.default.currentDirectoryPath + "/" + filename) else {
        fatalError("Invalid file path")
    }
    guard let fileContents = try? String(contentsOf: fileURL) else {
        fatalError("Could not read file")
    }
    
    let lines = fileContents.components(separatedBy: .newlines).filter { !$0.isEmpty }
    let dateFormatter = DateFormatter()
    dateFormatter.dateFormat = "yyyy-MM-dd HH:mm"
    
    var records: [Record] = []
    for line in lines {
        let parts = line.components(separatedBy: "] ")
        let timePart = parts[0].dropFirst()
        let actionPart = parts[1]
        
        guard let ts = dateFormatter.date(from: String(timePart)) else {
            fatalError("Could not parse date")
        }
        
        var guardID = -1
        var action = ""
        if actionPart.contains("Guard") {
            let scanner = Scanner(string: actionPart)
            scanner.scanString("Guard #", into: nil)
            scanner.scanInt(&guardID)
            action = "begins shift"
        } else if actionPart.contains("falls asleep") {
            action = "falls asleep"
        } else if actionPart.contains("wakes up") {
            action = "wakes up"
        }
        
        records.append(Record(timestamp: ts, action: action, guardID: guardID))
    }
    return records
}

let records = readAndParseInput(filename: "input.txt").sorted { $0.timestamp < $1.timestamp }

var guardSleepMinutes: [Int: [Int]] = [:]
var currentGuardID = 0
var sleepStart: Date?

for record in records {
    switch record.action {
    case "begins shift":
        currentGuardID = record.guardID
    case "falls asleep":
        sleepStart = record.timestamp
    case "wakes up":
        guard let start = sleepStart else { continue }
        let startMinute = Calendar.current.component(.minute, from: start)
        let endMinute = Calendar.current.component(.minute, from: record.timestamp)
        
        if guardSleepMinutes[currentGuardID] == nil {
            guardSleepMinutes[currentGuardID] = Array(repeating: 0, count: 60)
        }
        
        for i in startMinute..<endMinute {
            guardSleepMinutes[currentGuardID]![i] += 1
        }
    default:
        break
    }
}

var maxSleep = 0
var sleepiestGuard = 0

for (guardID, minutes) in guardSleepMinutes {
    let totalSleep = minutes.reduce(0, +)
    if totalSleep > maxSleep {
        maxSleep = totalSleep
        sleepiestGuard = guardID
    }
}

var maxMinute = 0
var maxMinuteCount = 0

if let sleepMinutes = guardSleepMinutes[sleepiestGuard] {
    for (i, count) in sleepMinutes.enumerated() {
        if count > maxMinuteCount {
            maxMinuteCount = count
            maxMinute = i
        }
    }
}

print(sleepiestGuard * maxMinute)
