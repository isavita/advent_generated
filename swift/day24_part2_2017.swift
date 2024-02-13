
import Foundation

func parseInput(_ input: String) -> [(Int, Int)] {
    return input.components(separatedBy: "\n").map {
        let ports = $0.components(separatedBy: "/").map { Int($0)! }
        return (ports[0], ports[1])
    }
}

func buildBridges(_ components: [(Int, Int)], _ currentPort: Int, _ usedComponents: Set<Int>) -> (Int, Int) {
    var maxStrength = 0
    var maxLength = 0
    var maxStrengthBridge = 0
    
    for (index, component) in components.enumerated() {
        if !usedComponents.contains(index) {
            var newComponents = components
            var newUsedComponents = usedComponents
            let (portA, portB) = component
            
            if portA == currentPort {
                newUsedComponents.insert(index)
                let (strength, length) = buildBridges(newComponents, portB, newUsedComponents)
                let bridgeStrength = currentPort + portB + strength
                
                if length + 1 > maxLength || (length + 1 == maxLength && bridgeStrength > maxStrengthBridge) {
                    maxStrength = bridgeStrength
                    maxLength = length + 1
                    maxStrengthBridge = bridgeStrength
                }
            } else if portB == currentPort {
                newUsedComponents.insert(index)
                let (strength, length) = buildBridges(newComponents, portA, newUsedComponents)
                let bridgeStrength = currentPort + portA + strength
                
                if length + 1 > maxLength || (length + 1 == maxLength && bridgeStrength > maxStrengthBridge) {
                    maxStrength = bridgeStrength
                    maxLength = length + 1
                    maxStrengthBridge = bridgeStrength
                }
            }
        }
    }
    
    return (maxStrength, maxLength)
}

let input = try String(contentsOfFile: "input.txt", encoding: .utf8)
let components = parseInput(input)
let result = buildBridges(components, 0, Set<Int>())
print(result.0)
