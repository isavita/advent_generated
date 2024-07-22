
import strutils, tables

# Directions: up, right, down, left
const directions = [(0, -1), (1, 0), (0, 1), (-1, 0)]

# Function to simulate the virus carrier's behavior
proc simulateVirusCarrier(grid: var Table[(int, int), bool], bursts: int): int =
    var x = 0
    var y = 0
    var direction = 0
    var infections = 0

    for _ in 0..<bursts:
        if grid.getOrDefault((x, y), false):  # Infected
            direction = (direction + 1) mod 4  # Turn right
            grid[(x, y)] = false  # Clean the node
        else:  # Clean
            direction = (direction + 3) mod 4  # Turn left
            grid[(x, y)] = true  # Infect the node
            infections += 1  # Count the infection

        # Move forward in the current direction
        x += directions[direction][0]
        y += directions[direction][1]

    return infections

# Main function to read input and run the simulation
proc main() =
    var grid = initTable[(int, int), bool]()
    var inputLines = readFile("input.txt").splitLines()
    
    # Initialize the grid from the input
    for i, line in inputLines:
        for j, char in line:
            if char == '#':
                grid[(j - inputLines.len div 2, i - inputLines.len div 2)] = true

    # Simulate the virus carrier for 10,000 bursts
    let result = simulateVirusCarrier(grid, 10000)
    echo result

# Run the main procedure
main()
