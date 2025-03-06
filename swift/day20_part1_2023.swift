
import Foundation

enum Pulse {
    case low, high
}

protocol Module {
    var name: String { get }
    var destinations: [String] { get set }
    func processPulse(from: String, pulse: Pulse) -> [(String, Pulse, String)]
}

class FlipFlop: Module {
    let name: String
    var destinations: [String]
    var isOn = false

    init(name: String, destinations: [String]) {
        self.name = name
        self.destinations = destinations
    }

    func processPulse(from: String, pulse: Pulse) -> [(String, Pulse, String)] {
        if pulse == .high {
            return []
        }

        isOn.toggle()
        let outgoingPulse: Pulse = isOn ? .high : .low
        return destinations.map { (name, outgoingPulse, $0) }
    }
}

class Conjunction: Module {
    let name: String
    var destinations: [String]
    var memory: [String: Pulse] = [:]

    init(name: String, destinations: [String]) {
        self.name = name
        self.destinations = destinations
    }

    func processPulse(from: String, pulse: Pulse) -> [(String, Pulse, String)] {
        memory[from] = pulse
        let outgoingPulse: Pulse = memory.values.allSatisfy { $0 == .high } ? .low : .high
        return destinations.map { (name, outgoingPulse, $0) }
    }
}

class Broadcaster: Module {
    let name: String
    var destinations: [String]

    init(name: String, destinations: [String]) {
        self.name = name
        self.destinations = destinations
    }

    func processPulse(from: String, pulse: Pulse) -> [(String, Pulse, String)] {
        return destinations.map { (name, pulse, $0) }
    }
}


func parseInput(from file: String) -> [String: Module] {
    var modules: [String: Module] = [:]
    var conjunctionInputs: [String: [String]] = [:]

    do {
        let contents = try String(contentsOfFile: file)
        let lines = contents.components(separatedBy: .newlines).filter { !$0.isEmpty }

        for line in lines {
            let parts = line.components(separatedBy: " -> ")
            let namePart = parts[0]
            let destinations = parts[1].components(separatedBy: ", ")

            if namePart == "broadcaster" {
                modules[namePart] = Broadcaster(name: namePart, destinations: destinations)
            } else if namePart.hasPrefix("%") {
                let name = String(namePart.dropFirst())
                modules[name] = FlipFlop(name: name, destinations: destinations)
            } else if namePart.hasPrefix("&") {
                let name = String(namePart.dropFirst())
                modules[name] = Conjunction(name: name, destinations: destinations)
                conjunctionInputs[name] = []
            }
        }
        
        // Populate conjunction inputs
         for line in lines {
            let parts = line.components(separatedBy: " -> ")
            let namePart = parts[0]
             let destinations = parts[1].components(separatedBy: ", ")
             let sourceName = namePart.hasPrefix("%") || namePart.hasPrefix("&") ? String(namePart.dropFirst()) : namePart
             
             for dest in destinations{
                 if let _ = modules[dest] as? Conjunction{
                     conjunctionInputs[dest]?.append(sourceName)
                 }
             }
        }

        for (conjunctionName, inputs) in conjunctionInputs {
            if let conjunction = modules[conjunctionName] as? Conjunction {
                for input in inputs {
                    conjunction.memory[input] = .low
                }
            }
        }

    } catch {
        print("Error reading file: \(error)")
    }

    return modules
}

func solve(modules: [String: Module]) -> Int {
    var lowPulses = 0
    var highPulses = 0

    for _ in 0..<1000 {
        // Button press sends a low pulse to broadcaster
        lowPulses += 1
        var queue: [(String, Pulse, String)] = [("button", .low, "broadcaster")]

        while !queue.isEmpty {
            let (source, pulse, destination) = queue.removeFirst()

            if let module = modules[destination] {
                let newPulses = module.processPulse(from: source, pulse: pulse)
                for (src, pl, dest) in newPulses {
                    if pl == .low {
                        lowPulses += 1
                    } else {
                        highPulses += 1
                    }
                    queue.append((src, pl, dest))
                }
            }
        }
    }
    return lowPulses * highPulses
}

func main() {
    let modules = parseInput(from: "input.txt")
    let result = solve(modules: modules)
    print(result)
}

main()

