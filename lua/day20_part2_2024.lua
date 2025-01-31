
local f = io.open("input.txt", "r")
local grid = {}
for line in f:lines() do
  grid[#grid + 1] = line
end
f:close()

local h = #grid
local w = #grid[1]
local S = { x = 0, y = 0 }
local E = { x = 0, y = 0 }
local walls = {}
for i = 1, h do
  walls[i] = {}
  for j = 1, w do
    walls[i][j] = false
  end
end
local trackCells = {}
for i = 1, h do
  for j = 1, w do
    local ch = grid[i]:sub(j, j)
    if ch == 'S' then
      S.x, S.y = i, j
    elseif ch == 'E' then
      E.x, E.y = i, j
    end
    if ch == '#' then
      walls[i][j] = true
    else
      trackCells[#trackCells + 1] = { x = i, y = j }
    end
  end
end

local dirs = { { 1, 0 }, { -1, 0 }, { 0, 1 }, { 0, -1 } }

local function isTrack(x, y)
  return x >= 1 and x <= h and y >= 1 and y <= w and not walls[x][y]
end

local function normalDistFrom(start)
  local dist = {}
  for i = 1, h do
    dist[i] = {}
    for j = 1, w do
      dist[i][j] = -1
    end
  end
  dist[start.x][start.y] = 0
  local q = { start }
  local head = 1
  while head <= #q do
    local cur = q[head]
    head = head + 1
    for i = 1, #dirs do
      local d = dirs[i]
      local nx, ny = cur.x + d[1], cur.y + d[2]
      if nx >= 1 and nx <= h and ny >= 1 and ny <= w and not walls[nx][ny] and dist[nx][ny] < 0 then
        dist[nx][ny] = dist[cur.x][cur.y] + 1
        q[#q + 1] = { x = nx, y = ny }
      end
    end
  end
  return dist
end

local distFromS = normalDistFrom(S)
local distFromE = normalDistFrom(E)
local normalCost
if distFromS[E.x][E.y] < 0 then
  print(0)
  return
else
  normalCost = distFromS[E.x][E.y]
end

local cheats = {}

for i = 1, #trackCells do
  local startPos = trackCells[i]
  local sd = distFromS[startPos.x][startPos.y]
  if sd >= 0 then
    local distC = {}
    for k = 1, h do
      distC[k] = {}
      for l = 1, w do
        distC[k][l] = -1
      end
    end
    distC[startPos.x][startPos.y] = 0
    local q = { startPos }
    local head = 1
    while head <= #q do
      local cur = q[head]
      head = head + 1
      local steps = distC[cur.x][cur.y]
      if steps == 20 then
        goto continue
      end
      for k = 1, #dirs do
        local d = dirs[k]
        local nx, ny = cur.x + d[1], cur.y + d[2]
        if nx >= 1 and nx <= h and ny >= 1 and ny <= w and distC[nx][ny] < 0 then
          distC[nx][ny] = steps + 1
          q[#q + 1] = { x = nx, y = ny }
        end
      end
      ::continue::
    end

    for x = 1, h do
      for y = 1, w do
        local s = distC[x][y]
        if s > 0 and s <= 20 and isTrack(x, y) then
          local ed = distFromE[x][y]
          if ed >= 0 then
            local cost = sd + s + ed
            if cost < normalCost then
              local key = { startPos.x, startPos.y, x, y }
              local old = cheats[key]
              if not old or cost < old then
                cheats[key] = cost
              end
            end
          end
        end
      end
    end
  end
end

local count = 0
for _, cost in pairs(cheats) do
  local saving = normalCost - cost
  if saving >= 100 then
    count = count + 1
  end
end
print(count)
