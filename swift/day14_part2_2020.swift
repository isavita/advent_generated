
import Foundation

// Function to apply the mask to an address
func applyMaskToAddress(address: Int, mask: String) -> [Int] {
    var maskedAddress = ""
    for (i, bit) in mask.enumerated() {
        if bit == "0" {
            maskedAddress.append(String(address >> (35 - i) & 1))
        } else {
            maskedAddress.append(bit)
        }
    }
    
    // Generate all possible addresses from the masked address
    return generateAddresses(maskedAddress)
}

// Recursive function to generate all possible addresses with floating bits
func generateAddresses(_ address: String) -> [Int] {
    var addresses: [Int] = []
    
    // Find the first 'X' to replace with '0' and '1'
    if let index = address.firstIndex(of: "X") {
        let prefix = String(address[..<index])
        let suffix = String(address[address.index(after: index)...])
        
        // Replace 'X' with '0' and '1'
        addresses += generateAddresses(prefix + "0" + suffix)
        addresses += generateAddresses(prefix + "1" + suffix)
    } else {
        // Convert the final address string to an integer
        if let finalAddress = Int(address, radix: 2) {
            addresses.append(finalAddress)
        }
    }
    
    return addresses
}

// Main function to execute the program
func executeProgram(from input: String) -> Int {
    var memory: [Int: Int] = [:]
    var currentMask: String = ""
    
    let lines = input.split(separator: "\n")
    
    for line in lines {
        if line.hasPrefix("mask") {
            // Update the current mask
            currentMask = String(line.split(separator: "=")[1].trimmingCharacters(in: .whitespaces))
        } else if line.hasPrefix("mem") {
            // Write to memory
            let parts = line.split(separator: "=")
            let addressPart = parts[0].trimmingCharacters(in: .whitespaces)
            let valuePart = parts[1].trimmingCharacters(in: .whitespaces)
            
            let address = Int(addressPart.dropFirst(4).dropLast(1))!
            let value = Int(valuePart)!
            
            // Apply the mask to the address
            let addresses = applyMaskToAddress(address: address, mask: currentMask)
            for addr in addresses {
                memory[addr] = value
            }
        }
    }
    
    // Calculate the sum of all values in memory
    return memory.values.reduce(0, +)
}

// Read input from file
if let input = try? String(contentsOfFile: "input.txt") {
    let result = executeProgram(from: input)
    print("Sum of all values left in memory: \(result)")
} else {
    print("Failed to read input file.")
}
