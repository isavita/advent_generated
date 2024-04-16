-- Function to read and process the input file
local function readInput(filename)
    local file = io.open(filename, "r")
    local instructions = nil
    local network = {}

    if not file then
        error("File not found: " .. filename)
    end

    -- Read the first line for navigation instructions
    instructions = file:read("*line")

    -- Read the rest of the file for the node mappings
    for line in file:lines() do
        local node, left, right = line:match("^(%w+) = %((%w+), (%w+)%)$")
        if node and left and right then
            network[node] = {left = left, right = right}
        end
    end

    file:close()
    return instructions, network
end

-- Function to navigate the network
local function navigateNetwork(instructions, network)
    local currentNode = "AAA"
    local stepCount = 0
    local instructionIndex = 1

    while currentNode ~= "ZZZ" do
        local direction = instructions:sub(instructionIndex, instructionIndex)
        local nextNode = network[currentNode][direction == "R" and "right" or "left"]
        currentNode = nextNode
        stepCount = stepCount + 1
        instructionIndex = (instructionIndex % #instructions) + 1
    end

    return stepCount
end

-- Main execution block
local function main()
    local instructions, network = readInput("input.txt")
    local steps = navigateNetwork(instructions, network)
    print("Steps required to reach ZZZ:", steps)
end

-- Run the main function
main()