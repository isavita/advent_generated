
import Foundation

// Function to evaluate a single expression
func evaluateExpression(_ expression: String) -> Int {
    var tokens = Array(expression.replacingOccurrences(of: " ", with: ""))
    var index = 0
    
    func parse() -> Int {
        var result = parseTerm()
        
        while index < tokens.count {
            let operatorToken = tokens[index]
            if operatorToken == "+" || operatorToken == "*" {
                index += 1
                let nextTerm = parseTerm()
                result = operatorToken == "+" ? result + nextTerm : result * nextTerm
            } else {
                break
            }
        }
        
        return result
    }
    
    func parseTerm() -> Int {
        if tokens[index] == "(" {
            index += 1 // Skip '('
            let result = parse()
            index += 1 // Skip ')'
            return result
        } else {
            let number = Int(String(tokens[index]))!
            index += 1
            return number
        }
    }
    
    return parse()
}

// Read input from the file and evaluate each expression
func evaluateExpressionsFromFile() -> Int {
    let fileURL = URL(fileURLWithPath: "input.txt")
    do {
        let content = try String(contentsOf: fileURL, encoding: .utf8)
        let expressions = content.split(separator: "\n")
        let results = expressions.map { evaluateExpression(String($0)) }
        return results.reduce(0, +) // Sum of all results
    } catch {
        print("Error reading file: \(error)")
        return 0
    }
}

// Main execution
let totalSum = evaluateExpressionsFromFile()
print(totalSum)
