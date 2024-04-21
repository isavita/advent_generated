import Foundation

var masses: [Int] = []
var total: Float = 0.0

func processLine(_ line: String) {
    if let m = Int(line.trimmingCharacters(in: .whitespaces)) {
        masses.append(m)
    } else {
        print("Error parsing line")
    }
}

func getTotal() {
    var tempTotal: Float = 0.0
    for m in masses {
        tempTotal += Float(floor(Float(m) / 3) - 2)
    }
    total = tempTotal
}

func main() {
    do {
        let fileContent = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let lines = fileContent.components(separatedBy: "\n")
        
        for line in lines {
            processLine(line)
        }
        
        getTotal()
        
        print(total)
    } catch {
        print("Error while reading file")
    }
}

main()