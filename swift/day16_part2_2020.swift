import Foundation

struct Rule {
    let name: String
    let ranges: [(Int, Int)]

    func isValid(_ value: Int) -> Bool {
        for range in ranges {
            if value >= range.0 && value <= range.1 {
                return true
            }
        }
        return false
    }
}

func parseTicket(_ line: String) -> [Int] {
    return line.components(separatedBy: ",").compactMap { Int($0) }
}

func isValidTicket(_ ticket: [Int], rules: [Rule]) -> Bool {
    for value in ticket {
        if !isValidForAnyRule(value, rules: rules) {
            return false
        }
    }
    return true
}

func isValidForAnyRule(_ value: Int, rules: [Rule]) -> Bool {
    for rule in rules {
        if rule.isValid(value) {
            return true
        }
    }
    return false
}

func solveFieldPositions(rules: [Rule], tickets: [[Int]]) -> [String: Int] {
    var validPositions: [String: [Int: Bool]] = [:]
    for rule in rules {
        validPositions[rule.name] = [:]
        for i in 0..<tickets[0].count {
            var valid = true
            for ticket in tickets {
                if !rule.isValid(ticket[i]) {
                    valid = false
                    break
                }
            }
            if valid {
                validPositions[rule.name]![i] = true
            }
        }
    }

    var fieldPositions: [String: Int] = [:]
    while fieldPositions.count < rules.count {
        for (name, positions) in validPositions {
            if positions.count == 1 {
                for (pos, _) in positions {
                    fieldPositions[name] = pos
                    for (otherName, otherPositions) in validPositions {
                        if otherName != name {
                            var mutableOtherPositions = otherPositions
                            mutableOtherPositions[pos] = nil
                            validPositions[otherName] = mutableOtherPositions
                        }
                    }
                }
                validPositions[name] = nil
            }
        }
    }
    return fieldPositions
}

func calculateDepartureProduct(ticket: [Int], fieldPositions: [String: Int]) -> Int {
    var product = 1
    for (name, pos) in fieldPositions {
        if name.hasPrefix("departure") {
            product *= ticket[pos]
        }
    }
    return product
}

let file = "input.txt"
do {
    let fileContent = try String(contentsOfFile: file, encoding: .utf8)
    let lines = fileContent.components(separatedBy: "\n")
    var rules: [Rule] = []
    var myTicket: [Int] = []
    var nearbyTickets: [[Int]] = []
    var section = 0

    for line in lines {
        if line.isEmpty {
            section += 1
            continue
        }
        switch section {
        case 0:
            let parts = line.components(separatedBy: ": ")
            if let rangeParts = parts.last?.components(separatedBy: " or ") {
                let rule = Rule(name: parts.first!, ranges: [
                    (Int(rangeParts[0].components(separatedBy: "-")[0])!, Int(rangeParts[0].components(separatedBy: "-")[1])!),
                    (Int(rangeParts[1].components(separatedBy: "-")[0])!, Int(rangeParts[1].components(separatedBy: "-")[1])!)
                ])
                rules.append(rule)
            }
        case 1:
            if line != "your ticket:" {
                myTicket = parseTicket(line)
            }
        case 2:
            if line != "nearby tickets:" {
                let ticket = parseTicket(line)
                if isValidTicket(ticket, rules: rules) {
                    nearbyTickets.append(ticket)
                }
            }
        default:
            break
        }
    }

    let fieldPositions = solveFieldPositions(rules: rules, tickets: nearbyTickets)
    let departureProduct = calculateDepartureProduct(ticket: myTicket, fieldPositions: fieldPositions)
    print(departureProduct)
} catch {
    print("Error reading file: \(error)")
}