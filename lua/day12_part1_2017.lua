-- Function to parse the input and build the adjacency list
local function parseInput(filename)
    local file = io.open(filename, "r")
    local graph = {}

    for line in file:lines() do
        local id, connections = line:match("(%d+) <%-> (.+)")
        id = tonumber(id)
        graph[id] = {}
        for conn in connections:gmatch("%d+") do
            table.insert(graph[id], tonumber(conn))
        end
    end

    file:close()
    return graph
end

-- Function to perform a Depth-First Search (DFS) to find all connected nodes
local function dfs(graph, start)
    local stack = {start}
    local visited = {}
    local count = 0

    while #stack > 0 do
        local node = table.remove(stack)
        if not visited[node] then
            visited[node] = true
            count = count + 1
            for _, neighbor in ipairs(graph[node]) do
                if not visited[neighbor] then
                    table.insert(stack, neighbor)
                end
            end
        end
    end

    return count
end

-- Main function to solve the problem
local function main()
    local graph = parseInput("input.txt")
    local result = dfs(graph, 0)
    print("Number of programs in the group that contains program ID 0:", result)
end

main()