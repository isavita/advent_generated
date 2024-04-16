-- The results from the MFCSAM
local mfcsam = {
    children = 3,
    cats = 7,
    samoyeds = 2,
    pomeranians = 3,
    akitas = 0,
    vizslas = 0,
    goldfish = 5,
    trees = 3,
    cars = 2,
    perfumes = 1
}

-- Function to parse a line and return the Aunt's number and attributes
local function parse_line(line)
    local attributes = {}
    local number = line:match("^Sue (%d+):")
    for key, value in line:gmatch("(%w+): (%d+)") do
        attributes[key] = tonumber(value)
    end
    return tonumber(number), attributes
end

-- Read the file and determine which Aunt Sue matches the MFCSAM output
local function find_matching_aunt(filename)
    local file = io.open(filename, "r")
    if not file then
        error("File could not be opened")
    end
    
    for line in file:lines() do
        local number, attributes = parse_line(line)
        local match = true
        for key, value in pairs(mfcsam) do
            if attributes[key] and attributes[key] ~= value then
                match = false
                break
            end
        end
        if match then
            file:close()
            return number
        end
    end
    
    file:close()
    return nil
end

-- Main execution
local filename = "input.txt"
local matching_aunt = find_matching_aunt(filename)
if matching_aunt then
    print("The Aunt Sue who gave the gift is number:", matching_aunt)
else
    print("No matching Aunt Sue found.")
end