
import Foundation

// Function to convert a hexadecimal string to a binary string
func hexToBinary(_ hex: String) -> String {
    let hexToBinaryMap: [Character: String] = [
        "0": "0000", "1": "0001", "2": "0010", "3": "0011",
        "4": "0100", "5": "0101", "6": "0110", "7": "0111",
        "8": "1000", "9": "1001", "A": "1010", "B": "1011",
        "C": "1100", "D": "1101", "E": "1110", "F": "1111"
    ]
    
    return hex.map { hexToBinaryMap[$0]! }.joined()
}

// Function to parse a packet and return the version sum and the remaining bits
func parsePacket(binary: String, startIndex: inout String.Index) -> Int {
    // Parse version
    let versionEnd = binary.index(startIndex, offsetBy: 3)
    let version = Int(binary[startIndex..<versionEnd], radix: 2)!
    startIndex = versionEnd

    // Parse type ID
    let typeIDEnd = binary.index(startIndex, offsetBy: 3)
    let typeID = Int(binary[startIndex..<typeIDEnd], radix: 2)!
    startIndex = typeIDEnd
    
    var versionSum = version

    // Handle literal value packet (Type ID 4)
    if typeID == 4 {
        var literalValueBinary = ""
        while true {
            let groupStart = startIndex
            let groupEnd = binary.index(groupStart, offsetBy: 5)
            let group = binary[groupStart..<groupEnd]
            literalValueBinary += group.dropFirst()
            startIndex = groupEnd
            if group.first == "0" {
                break
            }
        }
    } else { // Operator packet (Type ID other than 4)
        let lengthTypeID = binary[startIndex]
        startIndex = binary.index(after: startIndex)

        if lengthTypeID == "0" {
            // Total length in bits
            let lengthEnd = binary.index(startIndex, offsetBy: 15)
            let totalLength = Int(binary[startIndex..<lengthEnd], radix: 2)!
            startIndex = lengthEnd
            let subPacketsEndIndex = binary.index(startIndex, offsetBy: totalLength)

            while startIndex < subPacketsEndIndex {
                versionSum += parsePacket(binary: binary, startIndex: &startIndex)
            }
        } else {
            // Number of sub-packets
            let numSubPacketsEnd = binary.index(startIndex, offsetBy: 11)
            let numSubPackets = Int(binary[startIndex..<numSubPacketsEnd], radix: 2)!
            startIndex = numSubPacketsEnd

            for _ in 0..<numSubPackets {
                versionSum += parsePacket(binary: binary, startIndex: &startIndex)
            }
        }
    }
    return versionSum
}


// Main function
func main() {
    do {
        // Read input from file
        let input = try String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)

        // Convert hexadecimal to binary
        let binaryString = hexToBinary(input)

        // Parse the packet and calculate the version sum
        var startIndex = binaryString.startIndex
        let totalVersionSum = parsePacket(binary: binaryString, startIndex: &startIndex)

        // Print the result
        print(totalVersionSum)

    } catch {
        print("Error reading file: \(error)")
    }
}

// Call the main function to start the program
main()
