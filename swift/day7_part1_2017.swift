
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
let lines = input.components(separatedBy: .newlines)

var programsAbove: Set<String> = []
var programsBelow: Set<String> = []

for line in lines {
    let components = line.components(separatedBy: " -> ")
    let program = components[0].components(separatedBy: " ")[0]
    programsAbove.insert(program)
    
    if components.count > 1 {
        let abovePrograms = components[1].components(separatedBy: ", ")
        for aboveProgram in abovePrograms {
            programsBelow.insert(aboveProgram)
        }
    }
}

let bottomProgram = programsAbove.subtracting(programsBelow).first ?? "Unknown"
print(bottomProgram)
