
function readNodes(filename)
    local file = io.open(filename, "r")
    if not file then
        return {}
    end

    local nodes = {}
    local nodeRegex = "node%-x%d+%-y%d+%s+%d+T%s+(%d+)T%s+(%d+)T%s+%d+%%"
    for line in file:lines() do
        local used, avail = line:match(nodeRegex)
        if used and avail then
            table.insert(nodes, {used = tonumber(used), avail = tonumber(avail)})
        end
    end
    file:close()
    return nodes
end

function countViablePairs(nodes)
    local count = 0
    for i, a in ipairs(nodes) do
        for j, b in ipairs(nodes) do
            if i ~= j and a.used > 0 and a.used <= b.avail then
                count = count + 1
            end
        end
    end
    return count
end

local nodes = readNodes("input.txt")
local viablePairs = countViablePairs(nodes)
print(viablePairs)
