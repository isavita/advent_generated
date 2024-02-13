
import Foundation

struct Particle {
    var position: (Int, Int, Int)
    var velocity: (Int, Int, Int)
    var acceleration: (Int, Int, Int)
}

func parseInput(_ input: String) -> [Particle] {
    var particles: [Particle] = []
    
    let lines = input.components(separatedBy: .newlines)
    
    for line in lines {
        let components = line.components(separatedBy: ", ")
        
        let p = components[0].extractCoordinates()
        let v = components[1].extractCoordinates()
        let a = components[2].extractCoordinates()
        
        particles.append(Particle(position: p, velocity: v, acceleration: a))
    }
    
    return particles
}

extension String {
    func extractCoordinates() -> (Int, Int, Int) {
        let values = self.components(separatedBy: ["<", ">", ",", "="]).filter { !$0.isEmpty }.compactMap { Int($0) }
        return (values[0], values[1], values[2])
    }
}

func updateParticles(_ particles: inout [Particle]) {
    for i in 0..<particles.count {
        particles[i].velocity.0 += particles[i].acceleration.0
        particles[i].velocity.1 += particles[i].acceleration.1
        particles[i].velocity.2 += particles[i].acceleration.2
        
        particles[i].position.0 += particles[i].velocity.0
        particles[i].position.1 += particles[i].velocity.1
        particles[i].position.2 += particles[i].velocity.2
    }
}

func removeCollisions(_ particles: inout [Particle]) {
    var positions: [String: Int] = [:]
    var toRemove: [Int] = []
    
    for i in 0..<particles.count {
        let key = "\(particles[i].position.0),\(particles[i].position.1),\(particles[i].position.2)"
        
        if let index = positions[key] {
            toRemove.append(index)
            toRemove.append(i)
        } else {
            positions[key] = i
        }
    }
    
    toRemove = Array(Set(toRemove))
    toRemove.sort(by: >)
    
    for index in toRemove {
        particles.remove(at: index)
    }
}

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)

var particles = parseInput(input)

for _ in 0..<1000 {
    updateParticles(&particles)
    removeCollisions(&particles)
}

print(particles.count)
