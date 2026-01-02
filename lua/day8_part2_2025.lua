
local function read_points()
    local f = io.open("input.txt", "r")
    if not f then os.exit(1) end
    local pts = {}
    for line in f:lines() do
        line = line:match("^%s*(.-)%s*$")
        if #line > 0 then
            local x, y, z = line:match("^(%-?%d+),(%-?%d+),(%-?%d+)$")
            if x then pts[#pts+1] = {tonumber(x), tonumber(y), tonumber(z)} end
        end
    end
    f:close()
    return pts
end

local function dist_sq(a, b)
    local dx = a[1] - b[1]
    local dy = a[2] - b[2]
    local dz = a[3] - b[3]
    return dx*dx + dy*dy + dz*dz
end

local function find(par, x)
    while par[x] ~= x do
        par[x] = par[par[x]]
        x = par[x]
    end
    return x
end

local function unite(par, rank, a, b)
    a = find(par, a)
    b = find(par, b)
    if a == b then return false end
    if rank[a] < rank[b] then par[a] = b
    elseif rank[a] > rank[b] then par[b] = a
    else par[b] = a; rank[a] = rank[a] + 1 end
    return true
end

local pts = read_points()
local n = #pts
if n < 2 then return end

local edges = {}
for i = 1, n-1 do
    for j = i+1, n do
        edges[#edges+1] = {i, j, dist_sq(pts[i], pts[j])}
    end
end

table.sort(edges, function(a, b) return a[3] < b[3] end)

local parent = {}
local rank = {}
for i = 1, n do parent[i] = i; rank[i] = 0 end
local comps = n

for _, e in ipairs(edges) do
    local u, v = e[1], e[2]
    if unite(parent, rank, u, v) then
        comps = comps - 1
        if comps == 1 then
            local p1, p2 = pts[u], pts[v]
            print(string.format("Connected %d,%d,%d and %d,%d,%d", p1[1], p1[2], p1[3], p2[1], p2[2], p2[3]))
            print(string.format("Product of X coordinates: %d", p1[1] * p2[1]))
            break
        end
    end
end
