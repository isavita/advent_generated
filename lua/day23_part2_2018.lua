function abs(a)
    if a < 0 then
        return -a
    end
    return a
end

Coordinate = {}
Coordinate.__index = Coordinate

function Coordinate.new(x, y, z)
    local self = setmetatable({}, Coordinate)
    self.X = x
    self.Y = y
    self.Z = z
    return self
end

function Coordinate:distance(a)
    return abs(self.X - a.X) + abs(self.Y - a.Y) + abs(self.Z - a.Z)
end

Bots = {}
Bots.__index = Bots

function Bots.new()
    local self = setmetatable({}, Bots)
    self.data = {}
    return self
end

function Bots:add(c, r)
    if self.data[c] == nil then
        self.data[c] = {}
    end
    table.insert(self.data[c], r)
end

function Bots:haveInRange(pos)
    local sum = 0
    for c, rs in pairs(self.data) do
        for _, r in ipairs(rs) do
            if pos:distance(c) <= r then
                sum = sum + 1
            end
        end
    end
    return sum
end

function closestSuccess(bots)
    local Zero = Coordinate.new(0, 0, 0)
    local cur, topLeft, bottomRight = Coordinate.new(0, 0, 0), Coordinate.new(0, 0, 0), Coordinate.new(0, 0, 0)
    local zoom = 1 << (32 - 2)  -- Assuming 32-bit for simplicity in Lua example

    while true do
        local zoomedBots = Bots.new()
        local best = {pos = Coordinate.new(0, 0, 0), count = 0}

        for c, rs in pairs(bots.data) do
            for _, r in ipairs(rs) do
                local zc = Coordinate.new(math.floor(c.X / zoom), math.floor(c.Y / zoom), math.floor(c.Z / zoom))
                zoomedBots:add(zc, math.floor(r / zoom))
            end
        end

        for x = topLeft.X, bottomRight.X do
            for y = topLeft.Y, bottomRight.Y do
                for z = topLeft.Z, bottomRight.Z do
                    cur = Coordinate.new(x, y, z)
                    local c = zoomedBots:haveInRange(cur)

                    if c > best.count or (c == best.count and Zero:distance(cur) < Zero:distance(best.pos)) then
                        best.pos, best.count = cur, c
                    end
                end
            end
        end

        topLeft = Coordinate.new((best.pos.X - 1) << 1, (best.pos.Y - 1) << 1, (best.pos.Z - 1) << 1)
        bottomRight = Coordinate.new((best.pos.X + 1) << 1, (best.pos.Y + 1) << 1, (best.pos.Z + 1) << 1)
        zoom = zoom >> 1

        if zoom == 0 then
            return Zero:distance(best.pos)
        end
    end
end

function main()
    local file = io.open("input.txt", "r")
    local bots = Bots.new()
    for line in file:lines() do
        local x, y, z, r = line:match("pos=<(%d+),(%d+),(%d+)>, r=(%d+)")
        x, y, z, r = tonumber(x), tonumber(y), tonumber(z), tonumber(r)
        local coord = Coordinate.new(x, y, z)
        bots:add(coord, r)
    end
    file:close()

    print(closestSuccess(bots))
end

main()