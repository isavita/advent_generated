
local f = io.open("input.txt", "r")
local grid = {}
for line in f:lines() do
  table.insert(grid, line)
end
f:close()

local n, m = #grid, #grid[1]
local sx, sy, ex, ey
for i = 1, n do
  for j = 1, m do
    if grid[i]:sub(j, j) == 'S' then
      sx, sy = i, j
    elseif grid[i]:sub(j, j) == 'E' then
      ex, ey = i, j
    end
  end
end

local dx = {-1, 0, 1, 0}
local dy = {0, 1, 0, -1}

local dist = {}
for i = 1, n do
  dist[i] = {}
  for j = 1, m do
    dist[i][j] = {math.huge, math.huge, math.huge, math.huge}
  end
end
dist[sx][sy][2] = 0

local h = {}
local function push(v)
  table.insert(h, v)
  local i = #h
  while i > 1 do
    local p = math.floor((i - 1) / 2) + 1
    if h[p].cost <= h[i].cost then
      break
    end
    h[p], h[i] = h[i], h[p]
    i = p
  end
end

local function pop()
  local v = h[1]
  h[1] = h[#h]
  h[#h] = nil
  local i = 1
  while true do
    local l = 2 * i
    local r = 2 * i + 1
    local small = i
    if l <= #h and h[l].cost < h[small].cost then
      small = l
    end
    if r <= #h and h[r].cost < h[small].cost then
      small = r
    end
    if small == i then
      break
    end
    h[i], h[small] = h[small], h[i]
    i = small
  end
  return v
end

push({x = sx, y = sy, d = 2, cost = 0})

while #h > 0 do
  local u = pop()
  if dist[u.x][u.y][u.d] < u.cost then
    goto continue
  end
  if u.x == ex and u.y == ey then
    print(u.cost)
    return
  end
  for _, ndir in ipairs({(u.d) % 4 + 1, (u.d + 2) % 4 + 1}) do
    local nc = u.cost + 1000
    if nc < dist[u.x][u.y][ndir] then
      dist[u.x][u.y][ndir] = nc
      push({x = u.x, y = u.y, d = ndir, cost = nc})
    end
  end
  local nx, ny = u.x + dx[u.d], u.y + dy[u.d]
  if nx >= 1 and nx <= n and ny >= 1 and ny <= m and grid[nx]:sub(ny, ny) ~= '#' then
    local nc = u.cost + 1
    if nc < dist[nx][ny][u.d] then
      dist[nx][ny][u.d] = nc
      push({x = nx, y = ny, d = u.d, cost = nc})
    end
  end
  ::continue::
end
