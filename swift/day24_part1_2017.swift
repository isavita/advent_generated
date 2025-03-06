
import Foundation

struct Component {
    let port1: Int
    let port2: Int

    func strength() -> Int {
        return port1 + port2
    }
}

func findStrongestBridge(components: [Component], currentPort: Int, used: Set<Int>) -> Int {
    var maxStrength = 0

    for (index, component) in components.enumerated() {
        if !used.contains(index) {
            if component.port1 == currentPort || component.port2 == currentPort {
                let nextPort = component.port1 == currentPort ? component.port2 : component.port1
                var nextUsed = used
                nextUsed.insert(index)
                let strength = component.strength() + findStrongestBridge(components: components, currentPort: nextPort, used: nextUsed)
                maxStrength = max(maxStrength, strength)
            }
        }
    }

    return maxStrength
}
func findLongestBridge(components: [Component], currentPort: Int, used: Set<Int>) -> (Int, Int) {
    var maxLength = 0
    var maxStrength = 0

    for (index, component) in components.enumerated() {
        if !used.contains(index) {
            if component.port1 == currentPort || component.port2 == currentPort {
                let nextPort = (component.port1 == currentPort) ? component.port2 : component.port1
                var nextUsed = used
                nextUsed.insert(index)
                let (nextLength, nextStrength) = findLongestBridge(components: components, currentPort: nextPort, used: nextUsed)
                let currentLength = 1 + nextLength
                let currentStrength = component.strength() + nextStrength

                if currentLength > maxLength {
                    maxLength = currentLength
                    maxStrength = currentStrength
                } else if currentLength == maxLength && currentStrength > maxStrength{
                    maxStrength = currentStrength
                }
            }
        }
    }
    return (maxLength, maxStrength)
}

func main() {
    do {
        // Read from input.txt
        let input = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let lines = input.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: "\n")

        var components: [Component] = []
        for line in lines {
            let ports = line.components(separatedBy: "/").compactMap { Int($0) }
            if ports.count == 2 {
                components.append(Component(port1: ports[0], port2: ports[1]))
            }
        }

        // Part 1: Find the strongest bridge
        let strongest = findStrongestBridge(components: components, currentPort: 0, used: Set<Int>())
        print("Strongest bridge strength: \(strongest)")
        
        //Part 2
        let longest = findLongestBridge(components: components, currentPort: 0, used: Set<Int>())
        print("Longest bridge strength: \(longest.1)")

    } catch {
        print("Error reading file: \(error)")
    }
}

// Ensure the main function is called
main()

