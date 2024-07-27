
import Foundation

func manhattanDistance(from input: Int) -> Int {
    // Step 1: Find the layer
    var layer = 0
    while (2 * layer + 1) * (2 * layer + 1) < input {
        layer += 1
    }
    
    // Step 2: Find the maximum number in the layer
    let maxInLayer = (2 * layer + 1) * (2 * layer + 1)
    
    // Step 3: Calculate the position of the input number in the layer
    let layerSize = 2 * layer
    let offset = maxInLayer - input
    
    // Determine the side of the square where the number is located
    let side = offset / layerSize
    let positionInSide = offset % layerSize
    
    var x, y: Int
    
    // Calculate coordinates based on which side the number is on
    switch side {
    case 0: // bottom side
        x = layer
        y = -layer + positionInSide
    case 1: // left side
        x = layer - positionInSide
        y = layer
    case 2: // top side
        x = -layer
        y = layer - positionInSide
    case 3: // right side
        x = -layer + positionInSide
        y = -layer
    default:
        fatalError("Invalid side")
    }
    
    // Step 4: Calculate the Manhattan distance
    return abs(x) + abs(y)
}

// Read input from file
if let input = try? String(contentsOfFile: "input.txt", encoding: .utf8),
   let number = Int(input.trimmingCharacters(in: .whitespacesAndNewlines)) {
    let distance = manhattanDistance(from: number)
    print(distance)
} else {
    print("Failed to read input.")
}
