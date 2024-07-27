
import Foundation

// Define a structure to hold the sleep data for each guard
struct GuardSleepData {
    var id: Int
    var sleepMinutes: [Int: Int] // minute -> count of how many times asleep at that minute
    var totalSleep: Int
    
    init(id: Int) {
        self.id = id
        self.sleepMinutes = [:]
        self.totalSleep = 0
    }
    
    mutating func addSleep(from start: Int, to end: Int) {
        for minute in start..<end {
            sleepMinutes[minute, default: 0] += 1
            totalSleep += 1
        }
    }
}

// Function to parse the input and calculate the required result
func calculateGuardSleepData(from lines: [String]) -> Int {
    var guards: [Int: GuardSleepData] = [:]
    var currentGuardId: Int?
    var sleepStartMinute: Int?
    
    // Sort the lines to ensure chronological order
    let sortedLines = lines.sorted()
    
    for line in sortedLines {
        let components = line.split(separator: "]")
        guard components.count == 2 else { continue }
        
        let timestamp = String(components[0].dropFirst(1))
        let action = components[1].trimmingCharacters(in: .whitespaces)
        
        let dateFormatter = DateFormatter()
        dateFormatter.dateFormat = "yyyy-MM-dd HH:mm"
        guard let date = dateFormatter.date(from: timestamp) else { continue }
        
        let calendar = Calendar.current
        let minute = calendar.component(.minute, from: date)
        
        if action.contains("begins shift") {
            let guardIdString = action.split(separator: " ")[1]
            currentGuardId = Int(guardIdString.dropFirst(1))
            if currentGuardId != nil {
                if guards[currentGuardId!] == nil {
                    guards[currentGuardId!] = GuardSleepData(id: currentGuardId!)
                }
            }
        } else if action == "falls asleep" {
            sleepStartMinute = minute
        } else if action == "wakes up" {
            if let startMinute = sleepStartMinute, let guardId = currentGuardId {
                guards[guardId]?.addSleep(from: startMinute, to: minute)
            }
        }
    }
    
    // Now find the guard with the most frequent sleep minute
    var mostFrequentGuardId: Int?
    var mostFrequentMinute: Int = 0
    var maxSleepCount: Int = 0
    
    for guardData in guards.values {
        for (minute, count) in guardData.sleepMinutes {
            if count > maxSleepCount {
                maxSleepCount = count
                mostFrequentGuardId = guardData.id
                mostFrequentMinute = minute
            }
        }
    }
    
    // Calculate the result
    if let guardId = mostFrequentGuardId {
        return guardId * mostFrequentMinute
    }
    
    return 0
}

// Read input from the file
do {
    let fileURL = URL(fileURLWithPath: "input.txt")
    let input = try String(contentsOf: fileURL)
    let lines = input.components(separatedBy: .newlines).filter { !$0.isEmpty }
    
    let result = calculateGuardSleepData(from: lines)
    print(result)
} catch {
    print("Error reading file: \(error)")
}
