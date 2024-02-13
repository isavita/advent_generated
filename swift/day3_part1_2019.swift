
import Foundation

let input = try String(contentsOfFile: "input.txt")
let wires = input.components(separatedBy: .newlines)

func getCoordinates(for wire: String) -> Set<String> {
    var x = 0
    var y = 0
    var steps = 0
    var coordinates = Set<String>()
    
    for instruction in wire.components(separatedBy: ",") {
        let direction = instruction.first!
        let distance = Int(instruction.dropFirst())!
        
        for _ in 0..<distance {
            steps += 1
            switch direction {
            case "U":
                y += 1
            case "D":
                y -= 1
            case "L":
                x -= 1
            case "R":
                x += 1
            default:
                break
            }
            coordinates.insert("\(x),\(y)")
        }
    }
    
    return coordinates
}

let wire1 = getCoordinates(for: wires[0])
let wire2 = getCoordinates(for: wires[1])

let intersections = wire1.intersection(wire2)
let distances = intersections.map { abs(Int($0.components(separatedBy: ",").first!)!) + abs(Int($0.components(separatedBy: ",").last!)!) }

if let minDistance = distances.min() {
    print(minDistance)
}
