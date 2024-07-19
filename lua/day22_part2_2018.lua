
local function read_input(filename)
    local file = io.open(filename, "r")
    local content = file:read("*all")
    file:close()
    return content
end

local function parse_input(input)
    local depth, target_x, target_y = input:match("depth: (%d+)\ntarget: (%d+),(%d+)")
    return tonumber(depth), {x = tonumber(target_x), y = tonumber(target_y)}
end

local geologicY, geologicX, caveModulo = 16807, 48271, 20183
local TypeRocky, TypeWet, TypeNarrow = 0, 1, 2
local ToolNone, ToolTorch, ToolGear = 1, 2, 4

local Map = {}
Map.__index = Map

function Map:new(depth, target)
    local obj = {
        depth = depth,
        target = target,
        geologicIndicesCache = {},
        erosionLevelsCache = {}
    }
    setmetatable(obj, self)
    return obj
end

function Map:geologic_index(x, y)
    if self.geologicIndicesCache[y] and self.geologicIndicesCache[y][x] then
        return self.geologicIndicesCache[y][x]
    end
    self.geologicIndicesCache[y] = self.geologicIndicesCache[y] or {}

    local index
    if (x == 0 and y == 0) or (x == self.target.x and y == self.target.y) then
        index = 0
    elseif y == 0 then
        index = x * geologicY
    elseif x == 0 then
        index = y * geologicX
    else
        index = self:erosion_level(x - 1, y) * self:erosion_level(x, y - 1)
    end

    self.geologicIndicesCache[y][x] = index
    return index
end

function Map:erosion_level(x, y)
    if self.erosionLevelsCache[y] and self.erosionLevelsCache[y][x] then
        return self.erosionLevelsCache[y][x]
    end
    self.erosionLevelsCache[y] = self.erosionLevelsCache[y] or {}

    local level = (self:geologic_index(x, y) + self.depth) % caveModulo
    self.erosionLevelsCache[y][x] = level
    return level
end

function Map:type(x, y)
    return self:erosion_level(x, y) % 3
end

function Map:neighbors(pos, equip)
    local directions = {{1, 0}, {0, 1}, {-1, 0}, {0, -1}}
    local result = {}

    for _, dir in ipairs(directions) do
        local nx, ny = pos.x + dir[1], pos.y + dir[2]
        if nx >= 0 and ny >= 0 then
            local t = self:type(nx, ny)
            if equip & self:allowed(t) ~= 0 then
                table.insert(result, {pos = {x = nx, y = ny}, equip = equip, time = 1})
                table.insert(result, {pos = {x = nx, y = ny}, equip = equip ~ self:allowed(t), time = 8})
            end
        end
    end

    return result
end

function Map:allowed(regionType)
    if regionType == TypeRocky then
        return ToolGear | ToolTorch
    elseif regionType == TypeWet then
        return ToolGear | ToolNone
    elseif regionType == TypeNarrow then
        return ToolTorch | ToolNone
    end
end

local function rescue(input)
    local depth, target = parse_input(input)
    local m = Map:new(depth, target)

    local queue = {{pos = {x = 0, y = 0}, time = 0, equip = ToolTorch}}
    local distances = {}
    distances[0] = distances[0] or {}
    distances[0][0] = distances[0][0] or {}
    distances[0][0][ToolTorch] = 0

    while #queue > 0 do
        table.sort(queue, function(a, b) return a.time < b.time end)
        local item = table.remove(queue, 1)

        if item.pos.x == m.target.x and item.pos.y == m.target.y and item.equip == ToolTorch then
            return item.time
        end

        if item.pos.x > 8 * m.target.x or item.pos.y > 8 * m.target.y then
            goto continue
        end

        local key = string.format("%d,%d,%d", item.pos.x, item.pos.y, item.equip)
        if distances[item.pos.x] and distances[item.pos.x][item.pos.y] and distances[item.pos.x][item.pos.y][item.equip] and distances[item.pos.x][item.pos.y][item.equip] < item.time then
            goto continue
        end

        distances[item.pos.x] = distances[item.pos.x] or {}
        distances[item.pos.x][item.pos.y] = distances[item.pos.x][item.pos.y] or {}
        distances[item.pos.x][item.pos.y][item.equip] = item.time

        for _, n in ipairs(m:neighbors(item.pos, item.equip)) do
            local d = string.format("%d,%d,%d", n.pos.x, n.pos.y, n.equip)
            if not distances[n.pos.x] or not distances[n.pos.x][n.pos.y] or not distances[n.pos.x][n.pos.y][n.equip] or item.time + n.time < distances[n.pos.x][n.pos.y][n.equip] then
                distances[n.pos.x] = distances[n.pos.x] or {}
                distances[n.pos.x][n.pos.y] = distances[n.pos.x][n.pos.y] or {}
                distances[n.pos.x][n.pos.y][n.equip] = item.time + n.time
                table.insert(queue, {pos = n.pos, time = item.time + n.time, equip = n.equip})
            end
        end

        ::continue::
    end

    return 0
end

local input = read_input("input.txt")
print(rescue(input))
