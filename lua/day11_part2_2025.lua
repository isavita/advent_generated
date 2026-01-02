
local function read_input()
    local f = assert(io.open("input.txt", "r"))
    local lines = {}
    for line in f:lines() do
        line = line:gsub("%s+$", "")
        local p = line:match("^%s*(.-)%s*$")
        if p ~= "" then table.insert(lines, p) end
    end
    f:close()
    return lines
end

local name_to_id = {}
local adj = {}
local n = 0
local function idx(name)
    local id = name_to_id[name]
    if id then return id end
    n = n + 1
    name_to_id[name] = n
    adj[n] = {}
    return n
end

local function add_edge(u, v)
    table.insert(adj[u], v)
end

local function parse(lines)
    for _, line in ipairs(lines) do
        local src, dst = line:match("([^:]+):%s*(.+)")
        if src then
            local u = idx(src)
            for token in dst:gmatch("%S+") do
                local v = idx(token)
                add_edge(u, v)
            end
        end
    end
end

local function dfs(cur, tgt, memo)
    if cur == tgt then return 1 end
    if memo[cur] then return memo[cur] end
    local sum = 0
    for _, nb in ipairs(adj[cur]) do
        sum = sum + dfs(nb, tgt, memo)
    end
    memo[cur] = sum
    return sum
end

local function count_paths(s, t)
    return dfs(s, t, {})
end

local function main()
    local lines = read_input()
    parse(lines)
    local svr = idx("svr")
    local dac = idx("dac")
    local fft = idx("fft")
    local out = idx("out")
    local s1 = count_paths(svr, dac) * count_paths(dac, fft) * count_paths(fft, out)
    local s2 = count_paths(svr, fft) * count_paths(fft, dac) * count_paths(dac, out)
    print("Paths (svr->dac->fft->out): " .. s1)
    print("Paths (svr->fft->dac->out): " .. s2)
    print("Total paths visiting both: " .. (s1 + s2))
end

main()
