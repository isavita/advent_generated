
import Foundation

do {
    let input = try String(contentsOfFile: "input.txt")
    let layers = input.components(separatedBy: "\n")

    var severity = 0

    for layer in layers {
        let components = layer.components(separatedBy: ": ")
        let depth = Int(components[0])!
        let range = Int(components[1])!

        if depth % ((range - 1) * 2) == 0 {
            severity += depth * range
        }
    }

    print(severity)
} catch {
    print("Error reading the input file: \(error)")
}
