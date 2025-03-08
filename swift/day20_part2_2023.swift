
import Foundation

enum Pulse {
    case low
    case high
}

enum ModuleType {
    case flipFlop
    case conjunction
    case broadcast
}

protocol Module {
    var name: String { get }
    var type: ModuleType { get }
    var destinations: [String] { get }
    func processPulse(from source: String, pulse: Pulse) -> [(String, Pulse, String)]
}

class FlipFlop: Module {
    let name: String
    let type: ModuleType = .flipFlop
    let destinations: [String]
    var isOn: Bool = false

    init(name: String, destinations: [String]) {
        self.name = name
        self.destinations = destinations
    }

    func processPulse(from source: String, pulse: Pulse) -> [(String, Pulse, String)] {
        if pulse == .high {
            return []
        }

        isOn.toggle()
        let outputPulse: Pulse = isOn ? .high : .low
        return destinations.map { (destination) in (destination, outputPulse, self.name) }
    }
}

class Conjunction: Module {
    let name: String
    let type: ModuleType = .conjunction
    let destinations: [String]
    var memory: [String: Pulse] = [:]

    init(name: String, destinations: [String]) {
        self.name = name
        self.destinations = destinations
    }
    
    func addInput(input: String) {
        memory[input] = .low
    }

    func processPulse(from source: String, pulse: Pulse) -> [(String, Pulse, String)] {
        memory[source] = pulse

        let outputPulse: Pulse
        if memory.values.allSatisfy({ $0 == .high }) {
            outputPulse = .low
        } else {
            outputPulse = .high
        }
        return destinations.map { (destination) in (destination, outputPulse, self.name) }
    }
}

class Broadcast: Module {
    let name: String
    let type: ModuleType = .broadcast
    let destinations: [String]

    init(name: String, destinations: [String]) {
        self.name = name
        self.destinations = destinations
    }

    func processPulse(from source: String, pulse: Pulse) -> [(String, Pulse, String)] {
        return destinations.map { (destination) in (destination, pulse, self.name) }
    }
}


func parseInput(filePath: String) -> [String: Module] {
    guard let contents = try? String(contentsOfFile: filePath) else {
        fatalError("Could not read file \(filePath)")
    }

    var modules: [String: Module] = [:]
    var inputs: [String: [String]] = [:]

    for line in contents.split(separator: "\n") {
        let parts = line.split(separator: " -> ")
        let nameAndType = parts[0]
        let destinations = parts[1].split(separator: ", ").map { String($0) }

        let name: String
        let module: Module

        if nameAndType.starts(with: "%") {
            name = String(nameAndType.dropFirst())
            module = FlipFlop(name: name, destinations: destinations)
        } else if nameAndType.starts(with: "&") {
            name = String(nameAndType.dropFirst())
            module = Conjunction(name: name, destinations: destinations)
        } else {
            name = String(nameAndType)
            module = Broadcast(name: name, destinations: destinations)
        }
        modules[name] = module
        
        for dest in destinations {
            if inputs[dest] == nil {
                inputs[dest] = []
            }
            inputs[dest]?.append(name)
        }
    }
    
    for (name, module) in modules {
        if module.type == .conjunction {
            if let conjModule = module as? Conjunction {
                for input in inputs[name] ?? [] {
                    conjModule.addInput(input: input)
                }
            }
        }
    }

    return modules
}


func solvePart1(modules: [String: Module]) -> Int {
    var lowPulses = 0
    var highPulses = 0

    for _ in 0..<1000 {
        var queue: [(String, Pulse, String)] = [("broadcaster", .low, "button")]
        lowPulses += 1 // For the button press

        while !queue.isEmpty {
            let (destination, pulse, source) = queue.removeFirst()
            
            if pulse == .low {
                lowPulses += 1
            } else {
                highPulses += 1
            }

            guard let module = modules[destination] else { continue }
            let newPulses = module.processPulse(from: source, pulse: pulse)
            queue.append(contentsOf: newPulses)
        }
    }
    return lowPulses * highPulses
}

func solvePart2(modules: [String: Module]) -> Int {
    var cycleLengths: [String: Int] = [:]
    var buttonPresses = 0
    
    // Find the inputs to the conjunction module that feeds into "rx".
    var rxInput: String = ""
     for (name, module) in modules {
         if module.destinations.contains("rx") {
             rxInput = name
             break
         }
     }
    
    var feedInputs: [String] = []
    for (name, module) in modules {
         if module.destinations.contains(rxInput) {
             feedInputs.append(name)
         }
     }
    

    var modulesCopy = parseInput(filePath: "input.txt") // Reset modules to initial state
    
    while cycleLengths.count < feedInputs.count {
         buttonPresses += 1
        var queue: [(String, Pulse, String)] = [("broadcaster", .low, "button")]

        while !queue.isEmpty {
            let (destination, pulse, source) = queue.removeFirst()

            guard let module = modulesCopy[destination] else { continue }
            let newPulses = module.processPulse(from: source, pulse: pulse)
            queue.append(contentsOf: newPulses)
            
            for (nextDest, nextPulse, nextSrc) in newPulses {
                 if nextDest == rxInput && nextPulse == .high {
                     if !cycleLengths.keys.contains(nextSrc) {
                         cycleLengths[nextSrc] = buttonPresses
                     }
                 }
             }
        }
    }

    // Calculate the LCM (Least Common Multiple) of the cycle lengths
      func gcd(_ a: Int, _ b: Int) -> Int {
          if b == 0 {
              return a
          }
          return gcd(b, a % b)
      }

      func lcm(_ a: Int, _ b: Int) -> Int {
          return (a / gcd(a, b)) * b
      }
    
    let result = cycleLengths.values.reduce(1, {lcm($0,$1)})
    return result
}


func main() {
    let modules = parseInput(filePath: "input.txt")
    let part1Result = solvePart1(modules: modules)
    print("Part 1: \(part1Result)")
    let part2Result = solvePart2(modules: modules)
     print("Part 2: \(part2Result)")
}

main()
