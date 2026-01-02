local nodes = {}
local name_to_idx = {}
local function get_idx(name)
    local idx = name_to_idx[name]
    if not idx then
        idx = #nodes + 1
        name_to_idx[name] = idx
        nodes[idx] = {edges = {}}
    end
    return idx
end

local f = io.open("input.txt", "r")
if f then
    for line in f:lines() do
        local src, rest = line:match("^([^:]+):%s*(.*)$")
        if src and rest then
            local u = get_idx(src)
            for tok in rest:gmatch("%S+") do
                local v = get_idx(tok)
                table.insert(nodes[u].edges, v)
            end
        end
    end
    f:close()
end

local start = get_idx("you")
local finish = get_idx("out")
local memo = {}
local function dfs(u)
    if u == finish then return 1 end
    if memo[u] then return memo[u] end
    local total = 0
    for _, v in ipairs(nodes[u].edges) do
        total = total + dfs(v)
    end
    memo[u] = total
    return total
end

print(dfs(start))