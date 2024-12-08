
local function checkMAS(grid, x, y, dx, dy)
  local word = "MAS"
  local forward = true
  local backward = true
  for i = 0, #word - 1 do
    local newX, newY = x + dx * i, y + dy * i
    if newX < 1 or newY < 1 or newX > #grid or newY > #grid[1] or grid[newX]:sub(newY, newY) ~= word:sub(i + 1, i + 1) then
      forward = false
    end
    newX, newY = x + dx * i, y + dy * i
    if newX < 1 or newY < 1 or newX > #grid or newY > #grid[1] or grid[newX]:sub(newY, newY) ~= word:sub(#word - i, #word - i) then
      backward = false
    end
  end
  return forward or backward
end

local function checkXMAS(grid, x, y)
  return (checkMAS(grid, x - 1, y - 1, 1, 1) and checkMAS(grid, x - 1, y + 1, 1, -1)) or (checkMAS(grid, x + 1, y - 1, -1, 1) and checkMAS(grid, x + 1, y + 1, -1, -1))
end

local function countXMASPatterns(grid)
  local count = 0
  for i = 2, #grid - 1 do
    for j = 2, #grid[i] - 1 do
      if grid[i]:sub(j,j) == "A" and checkXMAS(grid, i, j) then
        count = count + 1
      end
    end
  end
  return count
end

local file = io.open("input.txt", "r")
if file == nil then
  print("Error opening file")
  os.exit(1)
end

local grid = {}
for line in file:lines() do
  table.insert(grid, line)
end
file:close()

local count = countXMASPatterns(grid)
print("X-MAS patterns appear " .. count .. " times in the word search")

