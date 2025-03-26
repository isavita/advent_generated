
local math = require("math")

local KIND_SPACE = 1 -- 1 << 0
local KIND_ELF = 2 -- 1 << 1
local KIND_GOBLIN = 4 -- 1 << 2
local KIND_WALL = 8 -- 1 << 3

local RUNE_KINDS = {
    ['.'] = KIND_SPACE,
    ['E'] = KIND_ELF,
    ['G'] = KIND_GOBLIN,
    ['#'] = KIND_WALL,
}

local OFFSETS = {
    { 0, -1 }, -- Up
    { -1, 0 }, -- Left
    { 1, 0 },  -- Right
    { 0, 1 },  -- Down
}

local DEFAULT_HITPOINTS = 200
local DEFAULT_POWER = 3

local Tile, Unit, Cave -- Forward declarations

-- Helper function: is_unit
local function is_unit(kind)
    return (kind == KIND_ELF or kind == KIND_GOBLIN)
end

-- Tile "Class"
Tile = {}
Tile.__index = Tile

function Tile.new(kind, x, y, map_ref)
    local self = setmetatable({}, Tile)
    self.kind = kind
    self.x = x
    self.y = y
    self.map = map_ref
    self.unit = nil
    return self
end

function Tile:walkable_neighbors()
    local neighbors = {}
    for _, offset in ipairs(OFFSETS) do
        local nx, ny = self.x + offset[1], self.y + offset[2]
        local row = self.map[ny]
        if row then
            local n = row[nx]
            if n and n.kind == KIND_SPACE then
                table.insert(neighbors, n)
            end
        end
    end
    return neighbors
end

-- Unit "Class"
Unit = {}
Unit.__index = Unit

function Unit.new(tile, kind, elf_power)
    local self = setmetatable({}, Unit)
    self.kind = kind
    self.hitpoints = DEFAULT_HITPOINTS
    self.power = (kind == KIND_ELF) and elf_power or DEFAULT_POWER
    self.tile = tile
    tile.unit = self
    return self
end

function Unit:targets(c)
    for _, unit in ipairs(c.units) do
        if unit.kind ~= self.kind and unit.hitpoints > 0 then
            return true
        end
    end
    return false
end

local function find_walkable_tiles(map, start)
    local frontier = { start }
    local distance = { [start] = 0 }
    local came_from = { [start] = nil }
    local head = 1

    while head <= #frontier do
        local current = frontier[head]
        head = head + 1

        for _, next_tile in ipairs(current:walkable_neighbors()) do
            if distance[next_tile] == nil then
                table.insert(frontier, next_tile)
                distance[next_tile] = distance[current] + 1
                came_from[next_tile] = current
            end
        end
    end
    return distance, came_from
end


function Unit:next_tile(c)
    local distances, came_from = find_walkable_tiles(c.map, self.tile)
    local enemies = {}
    for _, unit in ipairs(c.units) do
        if unit.kind ~= self.kind and unit.hitpoints > 0 then
            table.insert(enemies, unit)
        end
    end

    local targets = {}
    local closest_target_distance = math.huge

    for _, enemy in ipairs(enemies) do
        for _, target_tile in ipairs(enemy.tile:walkable_neighbors()) do
             local dist = distances[target_tile]
             if dist ~= nil then
                 if dist < closest_target_distance then
                     closest_target_distance = dist
                     targets = { target_tile }
                 elseif dist == closest_target_distance then
                     table.insert(targets, target_tile)
                 end
             end
        end
    end

    if #targets == 0 then
        return nil, nil
    end

    table.sort(targets, function(a, b)
        if a.y ~= b.y then return a.y < b.y end
        return a.x < b.x
    end)

    local final_target = targets[1]
    local current = final_target
    if came_from[current] == nil then -- Target is adjacent to start
        return final_target, final_target
    end

    while came_from[current] ~= self.tile do
        current = came_from[current]
        if current == nil then -- Should not happen if path exists
           return nil, nil
        end
    end
    return current, final_target
end

function Unit:enemy_neighbor(c)
    local target = nil
    local min_hp = math.huge

    for _, offset in ipairs(OFFSETS) do
        local nx, ny = self.tile.x + offset[1], self.tile.y + offset[2]
        local row = c.map[ny]
        if row then
            local t = row[nx]
            if t and t.unit and t.unit.kind ~= self.kind and t.unit.hitpoints > 0 then
                if t.unit.hitpoints < min_hp then
                    min_hp = t.unit.hitpoints
                    target = t.unit
                end
            end
        end
    end
    return target
end

function Unit:move(c)
    if self:enemy_neighbor(c) then
        return
    end

    local next_step, _ = self:next_tile(c)

    if next_step then
        self.tile.kind = KIND_SPACE
        self.tile.unit = nil
        self.tile = next_step
        self.tile.kind = self.kind
        self.tile.unit = self
    end
end

function Unit:attack(c)
    local enemy = self:enemy_neighbor(c)
    if enemy then
        local killed = enemy:damage(c, self.power)
        return killed and enemy.kind == KIND_ELF -- Return if an elf was killed
    end
    return false
end

function Unit:damage(c, damage_amount)
    self.hitpoints = self.hitpoints - damage_amount
    if self.hitpoints <= 0 then
        c:remove_unit(self)
        return true
    end
    return false
end


-- Cave "Class"
Cave = {}
Cave.__index = Cave

function Cave.new(input_lines, elf_power)
    local self = setmetatable({}, Cave)
    self.units = {}
    self.map = {}
    self:parse_map(input_lines, elf_power)
    return self
end

function Cave:parse_map(input_lines, elf_power)
    for y, row in ipairs(input_lines) do
        self.map[y] = self.map[y] or {}
        for x = 1, #row do
            local col = string.sub(row, x, x)
            local kind = RUNE_KINDS[col] or KIND_WALL
            local tile = Tile.new(kind, x, y, self.map)
            if is_unit(kind) then
                local unit = Unit.new(tile, kind, elf_power)
                table.insert(self.units, unit)
            end
            self.map[y][x] = tile
        end
    end
end

function Cave:status()
    local elves = false
    local goblins = false
    local hp = 0
    for _, u in ipairs(self.units) do
        if u.hitpoints > 0 then
            if u.kind == KIND_ELF then
                elves = true
            else
                goblins = true
            end
            hp = hp + u.hitpoints
        end
    end
    return hp, elves and goblins
end

function Cave:remove_the_dead()
    local living_units = {}
    for _, unit in ipairs(self.units) do
        if unit.hitpoints > 0 then
            table.insert(living_units, unit)
        end
    end
    self.units = living_units
end

function Cave:remove_unit(u)
     if u.tile then
        u.tile.kind = KIND_SPACE
        u.tile.unit = nil
        u.tile = nil -- Break reference
     end
     -- The unit will be fully removed from self.units in remove_the_dead
end

function Cave:tick(stop_on_elf_death)
    self:remove_the_dead() -- Remove units killed in previous round first

    table.sort(self.units, function(a, b)
        if a.tile.y ~= b.tile.y then return a.tile.y < b.tile.y end
        return a.tile.x < b.tile.x
    end)

    local units_this_round = {}
    for _, u in ipairs(self.units) do table.insert(units_this_round, u) end

    for _, unit in ipairs(units_this_round) do
        if unit.hitpoints <= 0 then
            -- Died earlier in this round
            goto continue -- Lua's equivalent of continue
        end

        if not unit:targets(self) then
            return false, false -- Combat ends immediately
        end

        unit:move(self)
        local elf_died = unit:attack(self)

        if elf_died and stop_on_elf_death then
            return false, true -- Combat ends, elf died
        end
        ::continue::
    end

    return true, false -- Round completed fully
end

-- Main combat logic
local function combat(input_lines)
    local cave = Cave.new(input_lines, DEFAULT_POWER)
    local rounds = 0
    while true do
        local hp, ongoing = cave:status()
        if not ongoing then
            return rounds * hp
        end

        local clean_round, _ = cave:tick(false) -- Part 1: don't stop on elf death

        if clean_round then
             rounds = rounds + 1
        end
    end
end

-- Entry point
local function main()
    local file = io.open("input.txt", "r")
    if not file then
        print("Error opening input.txt")
        return
    end

    local lines = {}
    for line in file:lines() do
        -- Basic trim equivalent
        line = line:match("^%s*(.-)%s*$")
        if #line > 0 then
             table.insert(lines, line)
        end
    end
    file:close()

    local result = combat(lines)
    print(result)
end

main()
