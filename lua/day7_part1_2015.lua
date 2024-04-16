function readFile(path)
    local file = io.open(path, "r")
    if not file then error("File not found") end
    local content = file:read("*a")
    file:close()
    return content
end

function someAssemblyRequired(input)
    local wireToRule = {}
    for line in input:gmatch("[^\r\n]+") do
        local parts = {}
        for part in line:gmatch("[^%s]+") do
            table.insert(parts, part)
        end
        local rule = table.concat(parts, " ", 1, #parts - 2)
        local wire = parts[#parts]
        wireToRule[wire] = rule
    end

    return memoDFS(wireToRule, "a", {})
end

function memoDFS(graph, entry, memo)
    if memo[entry] then return memo[entry] end
    if entry:match("^%d+$") then return tonumber(entry) end

    local sourceRule = graph[entry]
    local parts = {}
    for part in sourceRule:gmatch("[^%s]+") do
        table.insert(parts, part)
    end

    local result
    if #parts == 1 then
        result = memoDFS(graph, parts[1], memo)
    elseif parts[1] == "NOT" then
        local start = memoDFS(graph, parts[2], memo)
        result = (65535) ~ start
    elseif parts[2] == "AND" then
        result = memoDFS(graph, parts[1], memo) & memoDFS(graph, parts[3], memo)
    elseif parts[2] == "OR" then
        result = memoDFS(graph, parts[1], memo) | memoDFS(graph, parts[3], memo)
    elseif parts[2] == "LSHIFT" then
        result = memoDFS(graph, parts[1], memo) << memoDFS(graph, parts[3], memo)
    elseif parts[2] == "RSHIFT" then
        result = memoDFS(graph, parts[1], memo) >> memoDFS(graph, parts[3], memo)
    end

    memo[entry] = result
    return result
end

local input = readFile("input.txt")
print(someAssemblyRequired(input))