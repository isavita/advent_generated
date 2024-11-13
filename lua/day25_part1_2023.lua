
local function parseInput(input)
    local graph = {}
    for _, line in ipairs(input) do
        local parts = {}
        for part in line:gmatch("[^: ]+") do
            table.insert(parts, part)
        end
        local vertice = parts[1]
        graph[vertice] = graph[vertice] or {}
        for i = 2, #parts do
            local otherVertice = parts[i]
            graph[otherVertice] = graph[otherVertice] or {}
            graph[vertice][otherVertice] = true
            graph[otherVertice][vertice] = true
        end
    end
    return graph
end

local function breadthFirstSearch(graph, start, goalFunc)
    local frontier = {start}
    local reached = {[start] = true}
    local cameFrom = {[start] = start}
    while #frontier > 0 do
        local current = table.remove(frontier, 1)
        if goalFunc(current) then
            return true, cameFrom
        end
        for nextVertice in pairs(graph[current]) do
            if not reached[nextVertice] then
                table.insert(frontier, nextVertice)
                reached[nextVertice] = true
                cameFrom[nextVertice] = current
            end
        end
    end
    return false, cameFrom
end

local function reconstructPath(start, endVertice, cameFrom)
    local path = {}
    local current = endVertice
    while current ~= start do
        table.insert(path, 1, current)
        current = cameFrom[current]
    end
    table.insert(path, 1, start)
    return path
end

local function copyGraph(graph)
    local newGraph = {}
    for vertice, edges in pairs(graph) do
        newGraph[vertice] = {}
        for edge in pairs(edges) do
            newGraph[vertice][edge] = true
        end
    end
    return newGraph
end

local function solve(input)
    local minCut = 3
    local graph = parseInput(input)
    local source = next(graph)
    local separateGraph
    for endVertice in pairs(graph) do
        if source ~= endVertice then
            local newGraph = copyGraph(graph)
            for _ = 1, minCut do
                local _, cameFrom = breadthFirstSearch(newGraph, source, function(v) return v == endVertice end)
                local path = reconstructPath(source, endVertice, cameFrom)
                for j = 1, #path - 1 do
                    newGraph[path[j]][path[j + 1]] = nil
                end
            end
            local isValid, _ = breadthFirstSearch(newGraph, source, function(v) return v == endVertice end)
            if not isValid then
                separateGraph = newGraph
                break
            end
        end
    end
    local _, cameFrom = breadthFirstSearch(separateGraph, source, function() return false end)
    local length1 = 0
    for _ in pairs(cameFrom) do
        length1 = length1 + 1
    end
    local length2 = 0
    for _ in pairs(separateGraph) do
        length2 = length2 + 1
    end
    return length1 * (length2 - length1)
end

local function readFile(fileName)
    local file = io.open(fileName, "r")
    local lines = {}
    for line in file:lines() do
        table.insert(lines, line)
    end
    file:close()
    return lines
end

local input = readFile("input.txt")
print(solve(input))
