
local maxStrength = 0

local function findStrongestBridge(components, used, port, strength)
    if strength > maxStrength then
        maxStrength = strength
    end

    for i, c in ipairs(components) do
        if not used[i] then
            if c.a == port or c.b == port then
                used[i] = true
                local nextPort = c.a
                if c.a == port then
                    nextPort = c.b
                end
                findStrongestBridge(components, used, nextPort, strength + c.a + c.b)
                used[i] = false
            end
        end
    end
end

local components = {}
local file = io.open("input.txt", "r")
if not file then
    error("Could not open file")
end

for line in file:lines() do
    local ports = {}
    for port in string.gmatch(line, "%d+") do
        table.insert(ports, tonumber(port))
    end
    table.insert(components, {a = ports[1], b = ports[2]})
end
file:close()

local used = {}
for i = 1, #components do
    used[i] = false
end

findStrongestBridge(components, used, 0, 0)

print(maxStrength)
