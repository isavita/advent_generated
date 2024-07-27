
import Foundation

struct Particle {
    var position: (x: Int, y: Int, z: Int)
    var velocity: (x: Int, y: Int, z: Int)
    var acceleration: (x: Int, y: Int, z: Int)
    
    mutating func update() {
        // Update velocity based on acceleration
        velocity.x += acceleration.x
        velocity.y += acceleration.y
        velocity.z += acceleration.z
        
        // Update position based on velocity
        position.x += velocity.x
        position.y += velocity.y
        position.z += velocity.z
    }
    
    func manhattanDistance() -> Int {
        return abs(position.x) + abs(position.y) + abs(position.z)
    }
}

func parseParticle(line: String) -> Particle {
    let regex = try! NSRegularExpression(pattern: "p=<(-?\\d+),(-?\\d+),(-?\\d+)>, v=<(-?\\d+),(-?\\d+),(-?\\d+)>, a=<(-?\\d+),(-?\\d+),(-?\\d+)>")
    let nsString = line as NSString
    let results = regex.matches(in: line, range: NSRange(location: 0, length: nsString.length))
    
    guard let match = results.first else {
        fatalError("Invalid input line: \(line)")
    }
    
    let pX = Int(nsString.substring(with: match.range(at: 1)))!
    let pY = Int(nsString.substring(with: match.range(at: 2)))!
    let pZ = Int(nsString.substring(with: match.range(at: 3)))!
    let vX = Int(nsString.substring(with: match.range(at: 4)))!
    let vY = Int(nsString.substring(with: match.range(at: 5)))!
    let vZ = Int(nsString.substring(with: match.range(at: 6)))!
    let aX = Int(nsString.substring(with: match.range(at: 7)))!
    let aY = Int(nsString.substring(with: match.range(at: 8)))!
    let aZ = Int(nsString.substring(with: match.range(at: 9)))!
    
    return Particle(position: (pX, pY, pZ), velocity: (vX, vY, vZ), acceleration: (aX, aY, aZ))
}

func main() {
    let fileURL = URL(fileURLWithPath: "input.txt")
    
    do {
        let input = try String(contentsOf: fileURL, encoding: .utf8)
        let lines = input.split(separator: "\n").map(String.init)
        
        var particles = lines.map(parseParticle)
        
        // Simulate for a reasonable number of ticks (e.g., 1000)
        for _ in 0..<1000 {
            for i in 0..<particles.count {
                particles[i].update()
            }
        }
        
        // Find the particle with the minimum Manhattan distance
        let closestParticleIndex = particles.enumerated().min(by: { $0.element.manhattanDistance() < $1.element.manhattanDistance() })?.offset
        
        if let index = closestParticleIndex {
            print("The closest particle is \(index).")
        } else {
            print("No particles found.")
        }
        
    } catch {
        print("Error reading file: \(error)")
    }
}

main()
