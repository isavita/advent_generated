
local graph = {}
local nodes = {}
local bestClique = {}

-- Helper function to read the input file
local function readInput()
    local file = io.open("input.txt", "r")
    if not file then
        return
    end
    for line in file:lines() do
        local a, b = line:match("^([^-%s]+)-([^-%s]+)$")
        if a and b then
            if not graph[a] then
                graph[a] = {}
                table.insert(nodes, a)
            end
            if not graph[b] then
                graph[b] = {}
                table.insert(nodes, b)
            end
            graph[a][b] = true
            graph[b][a] = true
        end
    end
    file:close()
end

-- Helper function to sort and deduplicate nodes
local function prepareNodes()
    table.sort(nodes)
end

-- Helper functions for set operations
local function neighborsOf(node)
    return graph[node] or {}
end

local function intersect(a, b)
    local result = {}
    for _, x in ipairs(a) do
        if b[x] then
            table.insert(result, x)
        end
    end
    return result
end

local function union(a, x)
    local result = {}
    for _, elem in ipairs(a) do
        table.insert(result, elem)
    end
    table.insert(result, x)
    return result
end

local function remove(a, s)
    local result = {}
    for _, elem in ipairs(a) do
        if elem ~= s then
            table.insert(result, elem)
        end
    end
    return result
end

-- Bron–Kerbosch algorithm implementation
local function bronKerbosch(R, P, X)
    if #P == 0 and #X == 0 then
        if #R > #bestClique then
            bestClique = {}
            for _, v in ipairs(R) do
                table.insert(bestClique, v)
            end
        end
        return
    end

    local tempP = {}
    for _, v in ipairs(P) do
        table.insert(tempP, v)
    end

    for _, v in ipairs(tempP) do
        local neighbors = neighborsOf(v)
        local newP = intersect(P, neighbors)
        local newX = intersect(X, neighbors)
        bronKerbosch(union(R, v), newP, newX)
        P = remove(P, v)
        X = union(X, v)
    end
end

-- Main function
local function main()
    readInput()
    prepareNodes()
    bronKerbosch({}, nodes, {})
    table.sort(bestClique)
    print(table.concat(bestClique, ","))
end

main()
