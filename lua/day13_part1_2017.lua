
local file = io.open("input.txt", "r")
if not file then
    print("Error reading file")
    os.exit(1)
end

local firewall = {}
for line in file:lines() do
    local depth, rng = line:match("(%d+): (%d+)")
    firewall[tonumber(depth)] = {Range = tonumber(rng), Position = 0, Direction = 1}
end
file:close()

local severity = 0

local function maxDepth(firewall)
    local max = 0
    for depth in pairs(firewall) do
        if depth > max then
            max = depth
        end
    end
    return max
end

local function moveScanner(scanner)
    if scanner.Position == 0 then
        scanner.Direction = 1
    elseif scanner.Position == scanner.Range - 1 then
        scanner.Direction = -1
    end
    scanner.Position = scanner.Position + scanner.Direction
end

for depth = 0, maxDepth(firewall) do
    if firewall[depth] then
        if firewall[depth].Position == 0 then
            severity = severity + depth * firewall[depth].Range
        end
    end

    for _, scanner in pairs(firewall) do
        moveScanner(scanner)
    end
end

print(severity)
