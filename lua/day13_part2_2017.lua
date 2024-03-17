-- Read input from file
local file = io.open("input.txt", "r")
local firewall = {}
for line in file:lines() do
    local depth, range = line:match("(%d+): (%d+)")
    depth, range = tonumber(depth), tonumber(range)
    firewall[depth] = {range = range, position = 0, direction = 1}
end
file:close()

-- Function to check if the packet can pass through the firewall
local function pass_through(firewall, delay)
    for depth, scanner in pairs(firewall) do
        if (depth + delay) % (2 * (scanner.range - 1)) == 0 then
            return false
        end
    end
    return true
end

-- Find the minimum delay to pass through the firewall
local delay = 0
while not pass_through(firewall, delay) do
    delay = delay + 1
end

-- Print the answer
print(delay)