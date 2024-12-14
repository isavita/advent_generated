
local function gcd(a, b)
  if b == 0 then
    return a < 0 and -a or a
  end
  return gcd(b, a % b)
end

local file = io.open("input.txt", "r")
local grid = {}
for line in file:lines() do
  table.insert(grid, line)
end
file:close()

local h = #grid
local w = #grid[1]
local antennas = {}
for y = 1, h do
  for x = 1, w do
    local c = string.sub(grid[y], x, x)
    if c ~= "." then
      antennas[c] = antennas[c] or {}
      table.insert(antennas[c], {y, x})
    end
  end
end

local linesPerFreq = {}
for f, coords in pairs(antennas) do
  linesPerFreq[f] = {}
  local n = #coords
  for i = 1, n do
    for j = i + 1, n do
      local A = coords[i]
      local B = coords[j]
      local dy = B[1] - A[1]
      local dx = B[2] - A[2]
      local g = gcd(dy, dx)
      local sy = dy / g
      local sx = dx / g
      if sx < 0 or (sx == 0 and sy < 0) then
        sx = -sx
        sy = -sy
      end
      local c = sy * A[2] - sx * A[1]
      local key = sx .. "," .. sy .. "," .. c
      linesPerFreq[f][key] = true
    end
  end
end

local antinodes = {}
for _, lines in pairs(linesPerFreq) do
  for key, _ in pairs(lines) do
    local sx, sy, c = key:match("([^,]+),([^,]+),([^,]+)")
    sx = tonumber(sx)
    sy = tonumber(sy)
    c = tonumber(c)
    if sx == 0 and sy == 0 then
      goto continue
    end
    if sy == 0 then
      if c % sx == 0 then
        local y = -c / sx
        if y >= 1 and y <= h then
          for x = 1, w do
            antinodes[y .. "," .. x] = true
          end
        end
      end
    elseif sx == 0 then
      if c % sy == 0 then
        local x = c / sy
        if x >= 1 and x <= w then
          for y = 1, h do
            antinodes[y .. "," .. x] = true
          end
        end
      end
    else
      for y = 1, h do
        local val = c + sx * y
        if val % sy == 0 then
          local x = val / sy
          if x >= 1 and x <= w then
            antinodes[y .. "," .. x] = true
          end
        end
      end
    end
    ::continue::
  end
end

local count = 0
for _ in pairs(antinodes) do
  count = count + 1
end
print(count)
