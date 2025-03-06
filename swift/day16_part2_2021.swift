
import Foundation

// Function to convert a hexadecimal string to binary
func hexToBinary(_ hex: String) -> String {
    let hexToBinaryMap: [Character: String] = [
        "0": "0000", "1": "0001", "2": "0010", "3": "0011",
        "4": "0100", "5": "0101", "6": "0110", "7": "0111",
        "8": "1000", "9": "1001", "A": "1010", "B": "1011",
        "C": "1100", "D": "1101", "E": "1110", "F": "1111"
    ]
    
    return hex.map { hexToBinaryMap[$0]! }.joined()
}

// Packet structure
struct Packet {
    let version: Int
    let typeID: Int
    let value: Int
    let subpackets: [Packet]
}

// Function to parse a packet
func parsePacket(binary: String, startIndex: inout String.Index) -> Packet {
    // Parse version
    let version = Int(binary[startIndex..<binary.index(startIndex, offsetBy: 3)], radix: 2)!
    startIndex = binary.index(startIndex, offsetBy: 3)
    
    // Parse type ID
    let typeID = Int(binary[startIndex..<binary.index(startIndex, offsetBy: 3)], radix: 2)!
    startIndex = binary.index(startIndex, offsetBy: 3)
    
    if typeID == 4 {
        // Literal value
        var literalBinary = ""
        var isLastGroup = false
        while !isLastGroup {
            isLastGroup = binary[startIndex] == "0"
            startIndex = binary.index(after: startIndex)
            literalBinary += binary[startIndex..<binary.index(startIndex, offsetBy: 4)]
            startIndex = binary.index(startIndex, offsetBy: 4)
        }
        let value = Int(literalBinary, radix: 2)!
        return Packet(version: version, typeID: typeID, value: value, subpackets: [])
    } else {
        // Operator
        let lengthTypeID = binary[startIndex]
        startIndex = binary.index(after: startIndex)
        var subpackets: [Packet] = []
        
        if lengthTypeID == "0" {
            // Total length in bits
            let totalLength = Int(binary[startIndex..<binary.index(startIndex, offsetBy: 15)], radix: 2)!
            startIndex = binary.index(startIndex, offsetBy: 15)
            let subpacketsEndIndex = binary.index(startIndex, offsetBy: totalLength)
            while startIndex < subpacketsEndIndex {
                subpackets.append(parsePacket(binary: binary, startIndex: &startIndex))
            }
        } else {
            // Number of sub-packets
            let numSubpackets = Int(binary[startIndex..<binary.index(startIndex, offsetBy: 11)], radix: 2)!
            startIndex = binary.index(startIndex, offsetBy: 11)
            for _ in 0..<numSubpackets {
                subpackets.append(parsePacket(binary: binary, startIndex: &startIndex))
            }
        }
        
        // Calculate value based on operator type
        let value: Int
        switch typeID {
        case 0: // Sum
            value = subpackets.reduce(0) { $0 + $1.value }
        case 1: // Product
            value = subpackets.reduce(1) { $0 * $1.value }
        case 2: // Minimum
            value = subpackets.min(by: { $0.value < $1.value })?.value ?? 0
        case 3: // Maximum
            value = subpackets.max(by: { $0.value < $1.value })?.value ?? 0
        case 5: // Greater than
            value = subpackets[0].value > subpackets[1].value ? 1 : 0
        case 6: // Less than
            value = subpackets[0].value < subpackets[1].value ? 1 : 0
        case 7: // Equal to
            value = subpackets[0].value == subpackets[1].value ? 1 : 0
        default:
            value = 0
        }
        
        return Packet(version: version, typeID: typeID, value: value, subpackets: subpackets)
    }
}

// Function to calculate the sum of version numbers
func sumVersionNumbers(packet: Packet) -> Int {
    return packet.version + packet.subpackets.reduce(0) { $0 + sumVersionNumbers(packet: $1) }
}


func main() {
    do {
        // Read input from file
        let input = try String(contentsOfFile: "input.txt", encoding: .utf8).trimmingCharacters(in: .whitespacesAndNewlines)
        
        // Convert hex to binary
        let binary = hexToBinary(input)
        
        // Parse the packet
        var startIndex = binary.startIndex
        let packet = parsePacket(binary: binary, startIndex: &startIndex)
        
        // Calculate and print the sum of version numbers
        let versionSum = sumVersionNumbers(packet: packet)
        print("Sum of version numbers: \(versionSum)")

        // Print the value of the outermost packet
        print("Value of outermost packet: \(packet.value)")

    } catch {
        print("Error reading file: \(error)")
    }
}

// Ensure main is called
main()

