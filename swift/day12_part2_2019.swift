
import Foundation

struct Moon {
    var position: [Int]
    var velocity: [Int]
}

func applyGravity(moons: inout [Moon]) {
    for i in 0..<moons.count {
        for j in i+1..<moons.count {
            for k in 0..<3 {
                if moons[i].position[k] < moons[j].position[k] {
                    moons[i].velocity[k] += 1
                    moons[j].velocity[k] -= 1
                } else if moons[i].position[k] > moons[j].position[k] {
                    moons[i].velocity[k] -= 1
                    moons[j].velocity[k] += 1
                }
            }
        }
    }
}

func applyVelocity(moons: inout [Moon]) {
    for i in 0..<moons.count {
        for k in 0..<3 {
            moons[i].position[k] += moons[i].velocity[k]
        }
    }
}

func calculateEnergy(moons: [Moon]) -> Int {
    var totalEnergy = 0
    for moon in moons {
        let potentialEnergy = moon.position.map { abs($0) }.reduce(0, +)
        let kineticEnergy = moon.velocity.map { abs($0) }.reduce(0, +)
        totalEnergy += potentialEnergy * kineticEnergy
    }
    return totalEnergy
}

func findCycleLength(initial: [Moon]) -> Int {
    var moons = initial
    var cycles: [Int] = [0, 0, 0]
    var steps = 0
    
    while cycles.contains(0) {
        applyGravity(moons: &moons)
        applyVelocity(moons: &moons)
        steps += 1
        
        for i in 0..<3 {
            if cycles[i] == 0 {
                var match = true
                for j in 0..<moons.count {
                    if moons[j].position[i] != initial[j].position[i] || moons[j].velocity[i] != initial[j].velocity[i] {
                        match = false
                        break
                    }
                }
                if match {
                    cycles[i] = steps
                }
            }
        }
    }
    
    return lcm(cycles[0], lcm(cycles[1], cycles[2]))
}

func gcd(_ a: Int, _ b: Int) -> Int {
    if b == 0 {
        return a
    } else {
        return gcd(b, a % b)
    }
}

func lcm(_ a: Int, _ b: Int) -> Int {
    return a * b / gcd(a, b)
}

let inputURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: inputURL).trimmingCharacters(in: .whitespacesAndNewlines)

var moons: [Moon] = []
let regex = try NSRegularExpression(pattern: "<x=(-?\\d+), y=(-?\\d+), z=(-?\\d+)>")

for line in input.components(separatedBy: .newlines) {
    let matches = regex.matches(in: line, range: NSRange(line.startIndex..., in: line))
    let position = matches.map {
        (Int((line as NSString).substring(with: $0.range(at: 1)))!,
         Int((line as NSString).substring(with: $0.range(at: 2)))!,
         Int((line as NSString).substring(with: $0.range(at: 3)))!)
    }[0]
    moons.append(Moon(position: [position.0, position.1, position.2], velocity: [0, 0, 0]))
}

for _ in 0..<1000 {
    applyGravity(moons: &moons)
    applyVelocity(moons: &moons)
}

let totalEnergy = calculateEnergy(moons: moons)
print(totalEnergy)

let cycleLength = findCycleLength(initial: moons)
print(cycleLength)
