
local math = require("math")

local KIND_SPACE = 1
local KIND_ELF = 2
local KIND_GOBLIN = 4
local KIND_WALL = 8

local RUNE_KINDS = {
  ['.'] = KIND_SPACE,
  ['E'] = KIND_ELF,
  ['G'] = KIND_GOBLIN,
  ['#'] = KIND_WALL,
}

local function is_unit(bit)
  return (KIND_ELF | KIND_GOBLIN) & bit ~= 0
end

local OFFSETS = {
  {0, -1},
  {-1, 0},
  {1, 0},
  {0, 1},
}

local DEFAULT_HITPOINTS = 200
local DEFAULT_POWER = 3

local Tile = {}
Tile.__index = Tile

function Tile:new(kind, x, y, map)
  local self = setmetatable({}, Tile)
  self.kind = kind
  self.x = x
  self.y = y
  self.map = map
  self.unit = nil
  return self
end

function Tile:walkable_neighbors()
  local neighbors = {}
  for _, offset in ipairs(OFFSETS) do
    local n = self.map[self.y + offset[2]] and self.map[self.y + offset[2]][self.x + offset[1]]
    if n and n.kind == KIND_SPACE then
      table.insert(neighbors, n)
    end
  end
  return neighbors
end

local Unit = {}
Unit.__index = Unit

function Unit:new(tile, kind, elf_power)
  local self = setmetatable({}, Unit)
  self.kind = kind
  self.hitpoints = DEFAULT_HITPOINTS
  self.power = (kind ~= KIND_ELF) and DEFAULT_POWER or elf_power
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

local function find_walkable_tiles(map, t)
  local frontier = {t}
  local distance = {[t] = 0}
  local came_from = {[t] = nil}

  local visited = {}
  visited[t] = true

  while #frontier > 0 do
    local current = table.remove(frontier, 1)
    for _, next_tile in ipairs(current:walkable_neighbors()) do
      if not visited[next_tile] then
        visited[next_tile] = true
        table.insert(frontier, next_tile)
        distance[next_tile] = distance[current] + 1
        came_from[next_tile] = current
      end
    end
  end
  return distance, came_from
end

function Unit:next_tile(c)
  local targets = {}
  local closest_target_distance = math.huge
  local distances, path = find_walkable_tiles(c.map, self.tile)
  local enemies = self:enemies(c)

  for _, enemy in ipairs(enemies) do
    for _, target in ipairs(enemy.tile:walkable_neighbors()) do
      if distances[target] and distances[target] <= closest_target_distance then
        if distances[target] < closest_target_distance then
          closest_target_distance = distances[target]
          targets = {}
        end
        table.insert(targets, target)
      end
    end
  end

  table.sort(targets, function(a, b)
    if a.y ~= b.y then
      return a.y < b.y
    end
    return a.x < b.x
  end)

  if #targets > 0 then
    local target = targets[1]
    local current = target
    while path[current] ~= self.tile do
      current = path[current]
    end
    return current, target
  end
  return nil, nil
end

function Unit:enemies(c)
  local enemies = {}
  for _, unit in ipairs(c.units) do
    if unit.kind ~= self.kind and unit.hitpoints > 0 then
      table.insert(enemies, unit)
    end
  end

  table.sort(enemies, function(a, b)
    if a.tile.y ~= b.tile.y then
      return a.tile.y < b.tile.y
    end
    return a.tile.x < b.tile.x
  end)
  return enemies
end

function Unit:enemy_neighbor(c)
  local target = nil
  for _, offset in ipairs(OFFSETS) do
    local t = c.map[self.tile.y + offset[2]] and c.map[self.tile.y + offset[2]][self.tile.x + offset[1]]
    if t and t.unit and t.unit.kind ~= self.kind and t.unit.hitpoints > 0 then
      if not target or t.unit.hitpoints < target.hitpoints then
        target = t.unit
      end
    end
  end
  return target
end

function Unit:move(c)
  if self:enemy_neighbor(c) then
    return
  end
  local next_tile, _ = self:next_tile(c)
  if next_tile then
    next_tile.unit = self
    next_tile.kind = self.kind
    self.tile.kind = KIND_SPACE
    self.tile.unit = nil
    self.tile = next_tile
  end
end

function Unit:attack(c)
  local enemy = self:enemy_neighbor(c)
  if enemy then
    local killed = enemy:damage(c, self.power)
    return killed and enemy.kind == KIND_ELF
  end
  return false
end

function Unit:damage(c, damage)
  self.hitpoints = self.hitpoints - damage
  if self.hitpoints <= 0 then
    c:remove_unit(self)
    return true
  end
  return false
end

local Cave = {}
Cave.__index = Cave

function Cave:new(input, elf_power)
  local self = setmetatable({}, Cave)
  self.units = {}
  self.map = {}
  self:parse_map(input, elf_power)
  return self
end

function Cave:parse_map(input, elf_power)
  for y, row in ipairs(input) do
    self.map[y] = {}
    for x = 1, #row do
      local col = string.sub(row, x, x)
      local kind = RUNE_KINDS[col] or KIND_WALL
      local tile = Tile:new(kind, x, y, self.map)
      self.map[y][x] = tile
      if is_unit(kind) then
        table.insert(self.units, Unit:new(tile, kind, elf_power))
      end
    end
  end
end

function Cave:status()
  local elves = false
  local goblins = false
  local hp = 0
  for _, u in ipairs(self.units) do
    if u.hitpoints <= 0 then
      goto continue
    end
    if u.kind == KIND_ELF then
      elves = true
    else
      goblins = true
    end
    hp = hp + u.hitpoints
    ::continue::
  end
  return hp, elves and goblins
end

function Cave:remove_the_dead()
  local alive_units = {}
  for _, unit in ipairs(self.units) do
    if unit.hitpoints > 0 then
      table.insert(alive_units, unit)
    end
  end
  self.units = alive_units
end

function Cave:remove_unit(u)
  u.tile.kind = KIND_SPACE
  u.tile.unit = nil
  u.tile = nil
end

function Cave:tick(stop_on_elf_death)
  self:remove_the_dead()
  table.sort(self.units, function(a, b)
    if a.tile.y ~= b.tile.y then
      return a.tile.y < b.tile.y
    end
    return a.tile.x < b.tile.x
  end)

  for _, unit in ipairs(self.units) do
    if unit.hitpoints <= 0 then
      goto continue
    end
    if not unit:targets(self) then
      return false, false
    end
    unit:move(self)
    local elf_died = unit:attack(self)
    if elf_died and stop_on_elf_death then
      return false, true
    end
    ::continue::
  end
  return true, false
end

local function combat(input)
  local cave = Cave:new(input, DEFAULT_POWER)
  local i = 1
  while true do
    local hp, combat_ongoing = cave:status()
    if not combat_ongoing then
      return (i - 1) * hp
    end
    local clean_round, _ = cave:tick(false)
    if not clean_round then
      i = i - 1
    end
    i = i + 1
  end
end

local function cheating_elves(input)
  local elf_died = true
  local power = 4
  while elf_died do
    local cave = Cave:new(input, power)
    local i = 1
    while true do
      local hp, combat_ongoing = cave:status()
      if not combat_ongoing then
        return (i - 1) * hp
      end
      local clean_round, elf_died = cave:tick(true)
      if elf_died then
        break
      end
      if not clean_round then
        i = i - 1
      end
      i = i + 1
    end
    power = power + 1
  end
end

local function read_file(filename)
  local lines = {}
  local file = io.open(filename, "r")
  if file then
    for line in file:lines() do
      table.insert(lines, line)
    end
    file:close()
  else
    print("Error opening file: " .. filename)
    return nil
  end
  return lines
end

local function main()
  local lines = read_file("input.txt")
  if lines then
    print(cheating_elves(lines))
  end
end

main()
