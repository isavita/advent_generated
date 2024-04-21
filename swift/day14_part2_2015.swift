import Foundation

struct Reindeer {
    var speed: Int
    var flyTime: Int
    var restTime: Int
    var distance: Int
    var points: Int
    var flying: Bool
    var timeInMode: Int
}

func readReindeerDetails(from filename: String) -> [Reindeer]? {
    do {
        let content = try String(contentsOfFile: filename, encoding: .utf8)
        let lines = content.components(separatedBy: "\n")
        var reindeers: [Reindeer] = []
        for line in lines {
            let parts = line.components(separatedBy: " ")
            guard let speed = Int(parts[3]), let flyTime = Int(parts[6]), let restTime = Int(parts[13]) else { return nil }
            let reindeer = Reindeer(speed: speed, flyTime: flyTime, restTime: restTime, distance: 0, points: 0, flying: true, timeInMode: 0)
            reindeers.append(reindeer)
        }
        return reindeers
    } catch {
        return nil
    }
}

func simulateRaceWithPoints(reindeers: inout [Reindeer], totalSeconds: Int) {
    for _ in 0..<totalSeconds {
        var maxDistance = 0
        for i in reindeers.indices {
            if reindeers[i].flying {
                reindeers[i].distance += reindeers[i].speed
            }
            reindeers[i].timeInMode += 1
            if reindeers[i].flying && reindeers[i].timeInMode == reindeers[i].flyTime || !reindeers[i].flying && reindeers[i].timeInMode == reindeers[i].restTime {
                reindeers[i].flying.toggle()
                reindeers[i].timeInMode = 0
            }
            if reindeers[i].distance > maxDistance {
                maxDistance = reindeers[i].distance
            }
        }
        for i in reindeers.indices {
            if reindeers[i].distance == maxDistance {
                reindeers[i].points += 1
            }
        }
    }
}

func findMaxPoints(reindeers: [Reindeer]) -> Int {
    return reindeers.map { $0.points }.max() ?? 0
}

if let reindeerDetails = readReindeerDetails(from: "input.txt") {
    var reindeers = reindeerDetails // Declare as var
    simulateRaceWithPoints(reindeers: &reindeers, totalSeconds: 2503)
    let maxPoints = findMaxPoints(reindeers: reindeers)
    print(maxPoints)
} else {
    print("Error reading input")
}