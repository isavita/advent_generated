
local function loops(grid, sx, sy, sdir)
  local h = #grid
  local w = #grid[1]
  local dirs = {{0, -1}, {1, 0}, {0, 1}, {-1, 0}}
  local x, y, dir = sx, sy, sdir
  local seen = {}
  for step = 1, 2000000 do
    local st = {x = x, y = y, dir = dir}
    local key = string.format("%d,%d,%d", x, y, dir)
    if seen[key] then
      return true
    end
    seen[key] = true
    local dx, dy = dirs[dir + 1][1], dirs[dir + 1][2]
    local nx, ny = x + dx, y + dy
    if nx < 0 or nx >= w or ny < 0 or ny >= h then
      return false
    end
    if grid[ny + 1][nx + 1] == '#' then
      dir = (dir + 1) % 4
    else
      x, y = nx, ny
    end
  end
  return false
end

local f = io.open("input.txt", "r")
local grid = {}
for line in f:lines() do
  local row = {}
  for i = 1, #line do
    row[i] = line:sub(i, i)
  end
  grid[#grid + 1] = row
end
f:close()

local h = #grid
local w = #grid[1]
local startX, startY, startDir
for i = 1, h do
  for j = 1, w do
    if grid[i][j] == '^' then
      startX, startY, startDir = j - 1, i - 1, 0
    elseif grid[i][j] == '>' then
      startX, startY, startDir = j - 1, i - 1, 1
    elseif grid[i][j] == 'v' then
      startX, startY, startDir = j - 1, i - 1, 2
    elseif grid[i][j] == '<' then
      startX, startY, startDir = j - 1, i - 1, 3
    end
  end
end
grid[startY + 1][startX + 1] = '.'

local canLoop = 0
for y = 0, h - 1 do
  for x = 0, w - 1 do
    if x == startX and y == startY then
      goto continue
    end
    if grid[y + 1][x + 1] ~= '.' then
      goto continue
    end
    grid[y + 1][x + 1] = '#'
    if loops(grid, startX, startY, startDir) then
      canLoop = canLoop + 1
    end
    grid[y + 1][x + 1] = '.'
    ::continue::
  end
end

print(canLoop)
