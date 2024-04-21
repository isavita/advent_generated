import Foundation

func abs(_ x: Int) -> Int {
    return x < 0 ? -x : x
}

func max(_ a: Int, _ b: Int) -> Int {
    return a > b ? a : b
}

func distance(_ x: Int, _ y: Int, _ z: Int) -> Int {
    return (abs(x) + abs(y) + abs(z)) / 2
}

do {
    let fileContent = try String(contentsOfFile: "input.txt", encoding: .utf8)
    let directions = fileContent.components(separatedBy: ",")

    var x = 0, y = 0, z = 0
    var maxDistance = 0

    for dir in directions {
        switch dir {
        case "n":
            y += 1; z -= 1
        case "ne":
            x += 1; z -= 1
        case "se":
            x += 1; y -= 1
        case "s":
            y -= 1; z += 1
        case "sw":
            x -= 1; z += 1
        case "nw":
            x -= 1; y += 1
        default:
            break
        }

        let curDistance = distance(x, y, z)
        maxDistance = max(maxDistance, curDistance)
    }

    print(distance(x, y, z))
} catch {
    print("File reading error: \(error)")
}