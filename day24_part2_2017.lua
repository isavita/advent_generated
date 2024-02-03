
local maxStrength = 0
local maxLength = 0

local function findStrongestLongestBridge(components, used, port, strength, length)
    if length > maxLength or (length == maxLength and strength > maxStrength) then
        maxStrength = strength
        maxLength = length
    end

    for i, c in ipairs(components) do
        if not used[i] then
            if c[1] == port or c[2] == port then
                used[i] = true
                local nextPort = c[1]
                if c[1] == port then
                    nextPort = c[2]
                end
                findStrongestLongestBridge(components, used, nextPort, strength + c[1] + c[2], length + 1)
                used[i] = false
            end
        end
    end
end

local file = io.open("input.txt", "r")
if not file then
    error("Error opening file")
end

local components = {}
for line in file:lines() do
    local ports = {}
    for port in string.gmatch(line, "%d+") do
        table.insert(ports, tonumber(port))
    end
    table.insert(components, ports)
end

local used = {}
for i = 1, #components do
    used[i] = false
end

findStrongestLongestBridge(components, used, 0, 0, 0)

print(maxStrength)
