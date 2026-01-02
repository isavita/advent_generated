
local function find(p, x)
    while p[x] ~= x do
        p[x] = p[p[x]]
        x = p[x]
    end
    return x
end

local function union(p, sz, a, b)
    local ra, rb = find(p, a), find(p, b)
    if ra == rb then return end
    if sz[ra] < sz[rb] then ra, rb = rb, ra end
    p[rb] = ra
    sz[ra] = sz[ra] + sz[rb]
end

local pts = {}
for line in io.lines("input.txt") do
    local x, y, z = line:match("^%s*(-?%d+)%s*,%s*(-?%d+)%s*,%s*(-?%d+)%s*$")
    if x then pts[#pts+1] = {tonumber(x), tonumber(y), tonumber(z)} end
end
local n = #pts
if n < 2 then
    print("Not enough points to form circuits.")
    return
end

local edges = {}
for i = 1, n - 1 do
    local xi, yi, zi = pts[i][1], pts[i][2], pts[i][3]
    for j = i + 1, n do
        local dx = xi - pts[j][1]
        local dy = yi - pts[j][2]
        local dz = zi - pts[j][3]
        edges[#edges+1] = {i, j, dx*dx + dy*dy + dz*dz}
    end
end

table.sort(edges, function(a, b) return a[3] < b[3] end)

local parent, sz = {}, {}
for i = 1, n do parent[i], sz[i] = i, 1 end

local limit = #edges < 1000 and #edges or 1000
for i = 1, limit do
    local e = edges[i]
    union(parent, sz, e[1], e[2])
end

local top1, top2, top3 = 0, 0, 0
for i = 1, n do
    if parent[i] == i then
        local s = sz[i]
        if s > top1 then
            top3, top2, top1 = top2, top1, s
        elseif s > top2 then
            top3, top2 = top2, s
        elseif s > top3 then
            top3 = s
        end
    end
end

local result = 1
if top1 > 0 then result = result * top1 end
if top2 > 0 then result = result * top2 end
if top3 > 0 then result = result * top3 end

print("Product of three largest circuit sizes: " .. result)
