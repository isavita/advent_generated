local function abs(x)
    return (x < 0) and -x or x
end

local function manhattanDistance(a, b)
    return abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z) + abs(a.t - b.t)
end

local UnionFind = {}
UnionFind.__index = UnionFind

function UnionFind:new(size)
    local obj = setmetatable({}, UnionFind)
    obj.parent = {}
    for i = 1, size do
        obj.parent[i] = i
    end
    return obj
end

function UnionFind:find(x)
    if self.parent[x] ~= x then
        self.parent[x] = self:find(self.parent[x])
    end
    return self.parent[x]
end

function UnionFind:union(x, y)
    local rootX = self:find(x)
    local rootY = self:find(y)
    if rootX ~= rootY then
        self.parent[rootX] = rootY
    end
end

local function main()
    local file = io.open("input.txt", "r")
    local points = {}
    for line in file:lines() do
        local x, y, z, t = line:match("(-?%d+),(-?%d+),(-?%d+),(-?%d+)")
        x, y, z, t = tonumber(x), tonumber(y), tonumber(z), tonumber(t)
        table.insert(points, {x = x, y = y, z = z, t = t})
    end
    file:close()

    local uf = UnionFind:new(#points)
    for i = 1, #points do
        for j = 1, #points do
            if manhattanDistance(points[i], points[j]) <= 3 then
                uf:union(i, j)
            end
        end
    end

    local constellationCount = 0
    for i, parent in ipairs(uf.parent) do
        if i == parent then
            constellationCount = constellationCount + 1
        end
    end
    print(constellationCount)
end

main()