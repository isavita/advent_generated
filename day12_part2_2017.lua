
local file = io.open("input.txt", "r")
local adj = {}
local visited = {}
local groups = 0

for line in file:lines() do
    local parts = {}
    for part in line:gmatch("[%w]+") do
        table.insert(parts, part)
    end
    local from = tonumber(parts[1])
    local toNodes = {}
    for i = 2, #parts do
        table.insert(toNodes, tonumber(parts[i]))
        if adj[tonumber(parts[i])] == nil then
            adj[tonumber(parts[i])] = {}
        end
        table.insert(adj[tonumber(parts[i])], from)
    end
    if adj[from] == nil then
        adj[from] = {}
    end
    for _, to in ipairs(toNodes) do
        table.insert(adj[from], to)
        table.insert(adj[to], from)
    end
end

local function DFS(node)
    visited[node] = true
    for _, neighbor in ipairs(adj[node]) do
        if not visited[neighbor] then
            DFS(neighbor)
        end
    end
end

for node in pairs(adj) do
    if not visited[node] then
        DFS(node)
        groups = groups + 1
    end
end

print(groups)
