
local computers = {}
local graph = {}

-- Read input file
local file = io.open("input.txt", "r")
if not file then
    print("Error opening file")
    return
end

-- Build graph
for line in file:lines() do
    line = line:gsub("%s+", "")  -- Remove whitespace
    local a, b = line:match("(%w+)-(%w+)")
    if a and b then
        -- Add both directions
        computers[a] = true
        computers[b] = true
        if not graph[a] then graph[a] = {} end
        if not graph[b] then graph[b] = {} end
        graph[a][b] = true
        graph[b][a] = true
    end
end

file:close()

-- Collect all computer names
local compList = {}
for comp in pairs(computers) do
    table.insert(compList, comp)
end

local triplets = 0
local seen = {}

-- Iterate all possible triplets
for i = 1, #compList do
    for j = i + 1, #compList do
        for k = j + 1, #compList do
            local a, b, c = compList[i], compList[j], compList[k]
            if graph[a][b] and graph[b][c] and graph[a][c] then
                -- Check if any starts with 't'
                local hasT = a:sub(1,1):lower() == 't' or b:sub(1,1):lower() == 't' or c:sub(1,1):lower() == 't'
                if hasT then
                    -- Create sorted key
                    local sorted = {a, b, c}
                    table.sort(sorted)
                    local key = table.concat(sorted, ",")
                    if not seen[key] then
                        triplets = triplets + 1
                        seen[key] = true
                    end
                end
            end
        end
    end
end

print(triplets)
