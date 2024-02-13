
import Foundation

struct Reindeer {
    let name: String
    let speed: Int
    let flyTime: Int
    let restTime: Int
}

func parseInput(_ input: String) -> [Reindeer] {
    var reindeerList: [Reindeer] = []
    
    let lines = input.components(separatedBy: .newlines)
    
    for line in lines {
        let components = line.components(separatedBy: .whitespaces)
        let name = components[0]
        let speed = Int(components[3])!
        let flyTime = Int(components[6])!
        let restTime = Int(components[13])!
        
        let reindeer = Reindeer(name: name, speed: speed, flyTime: flyTime, restTime: restTime)
        reindeerList.append(reindeer)
    }
    
    return reindeerList
}

func distanceTraveled(_ reindeer: Reindeer, _ time: Int) -> Int {
    let cycleTime = reindeer.flyTime + reindeer.restTime
    let cycles = time / cycleTime
    let remainingTime = time % cycleTime
    
    let distance = cycles * reindeer.speed * reindeer.flyTime
    let remainingDistance = min(reindeer.flyTime, remainingTime) * reindeer.speed
    
    return distance + remainingDistance
}

let input = try String(contentsOfFile: "input.txt")
let reindeerList = parseInput(input)

var maxDistance = 0
for reindeer in reindeerList {
    let distance = distanceTraveled(reindeer, 2503)
    if distance > maxDistance {
        maxDistance = distance
    }
}

print(maxDistance)
