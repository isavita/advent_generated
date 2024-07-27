
import Foundation

// Function to read input from a file and parse the firewall configuration
func readFirewallConfiguration(from file: String) -> [(depth: Int, range: Int)] {
    var firewall = [(depth: Int, range: Int)]()
    
    do {
        let contents = try String(contentsOfFile: file)
        let lines = contents.split(separator: "\n")
        
        for line in lines {
            let parts = line.split(separator: ":").map { String($0).trimmingCharacters(in: .whitespaces) }
            if let depth = Int(parts[0]), let range = Int(parts[1]) {
                firewall.append((depth: depth, range: range))
            }
        }
    } catch {
        print("Error reading file: \(error)")
    }
    
    return firewall
}

// Function to check if the packet is caught at a given delay
func isCaught(firewall: [(depth: Int, range: Int)], delay: Int) -> Bool {
    for (depth, range) in firewall {
        let time = depth + delay
        let cycle = 2 * (range - 1)
        if time % cycle == 0 {
            return true // Caught by the scanner
        }
    }
    return false // Not caught
}

// Function to find the minimum delay required to pass through the firewall safely
func findMinimumDelay(firewall: [(depth: Int, range: Int)]) -> Int {
    var delay = 0
    while isCaught(firewall: firewall, delay: delay) {
        delay += 1
    }
    return delay
}

// Main program execution
let firewall = readFirewallConfiguration(from: "input.txt")
let minimumDelay = findMinimumDelay(firewall: firewall)
print("The minimum delay needed to pass through the firewall without being caught is: \(minimumDelay)")
