
local file = io.open("input.txt", "r")
local input = file:read("*all")
file:close()

local wireToRule = {}

for inst in input:gmatch("[^\n]+") do
    local parts = {}
    for part in inst:gmatch("%S+") do
        table.insert(parts, part)
    end
    wireToRule[parts[#parts]] = table.concat(parts, " ", 1, #parts-2)
end

local function memoDFS(graph, entry, memo)
    if memo[entry] then
        return memo[entry]
    end

    if tonumber(entry) then
        return tonumber(entry)
    end

    local sourceRule = graph[entry]
    local parts = {}
    for part in sourceRule:gmatch("%S+") do
        table.insert(parts, part)
    end

    local result
    if #parts == 1 then
        result = memoDFS(graph, parts[1], memo)
    elseif parts[1] == "NOT" then
        local start = memoDFS(graph, parts[2], memo)
        result = 65535 - start
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

local aSignal = memoDFS(wireToRule, "a", {})

wireToRule["b"] = tostring(aSignal)
print(memoDFS(wireToRule, "a", {}))
