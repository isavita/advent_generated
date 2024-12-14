
local function mod(a, b)
  return (a % b + b) % b
end

local function parseLine(line)
  local x, y, vx, vy = line:match("p=(-?%d+),(-?%d+) v=(-?%d+),(-?%d+)")
  return {tonumber(x), tonumber(y), tonumber(vx), tonumber(vy)}
end

local function moveRobots(robots, sizeX, sizeY)
  for _, robot in ipairs(robots) do
    robot[1] = mod(robot[1] + robot[3], sizeX)
    robot[2] = mod(robot[2] + robot[4], sizeY)
  end
end

local function countQuadrants(robots, sizeX, sizeY)
  local counts = {0, 0, 0, 0}
  local centerX = sizeX / 2
  local centerY = sizeY / 2
  for _, robot in ipairs(robots) do
    local x, y = robot[1], robot[2]
    if x < centerX then
      if y < centerY then
        counts[1] = counts[1] + 1
      elseif y > centerY then
        counts[2] = counts[2] + 1
      end
    elseif x > centerX then
      if y < centerY then
        counts[3] = counts[3] + 1
      elseif y > centerY then
        counts[4] = counts[4] + 1
      end
    end
  end
  return counts
end

local function hasNoOverlaps(robots)
  local positions = {}
  for _, robot in ipairs(robots) do
    local pos = {robot[1], robot[2]}
    local key = pos[1] .. "," .. pos[2]
    if positions[key] then
      return false
    end
    positions[key] = true
  end
  return true
end

local function drawGrid(robots, sizeX, sizeY)
  local grid = {}
  for y = 0, sizeY - 1 do
    grid[y] = string.rep(".", sizeX)
  end
  for _, robot in ipairs(robots) do
    local x, y = robot[1], robot[2]
    grid[y] = grid[y]:sub(1, x) .. "#" .. grid[y]:sub(x + 2)
  end
  for _, line in ipairs(grid) do
    print(line)
  end
end

local sizeX = 101
local sizeY = 103
local robots = {}
local file = io.open("input.txt", "r")
if file then
  for line in file:lines() do
    if line ~= "" then
      table.insert(robots, parseLine(line))
    end
  end
  file:close()
else
  print("Error opening input.txt")
  os.exit(1)
end

local robotsPart1 = {}
for _, robot in ipairs(robots) do
  table.insert(robotsPart1, {robot[1], robot[2], robot[3], robot[4]})
end

for n = 1, 100 do
  moveRobots(robotsPart1, sizeX, sizeY)
end

local counts = countQuadrants(robotsPart1, sizeX, sizeY)
local safetyFactor = 1
for _, c in ipairs(counts) do
  safetyFactor = safetyFactor * c
end
print("Part 1 - Safety Factor after 100 seconds:", safetyFactor)

local robotsPart2 = {}
for _, robot in ipairs(robots) do
  table.insert(robotsPart2, {robot[1], robot[2], robot[3], robot[4]})
end

local seconds = 0
while true do
  if hasNoOverlaps(robotsPart2) then
    break
  end
  moveRobots(robotsPart2, sizeX, sizeY)
  seconds = seconds + 1
  if seconds > 1000000 then
    print("Exceeded maximum iterations without finding a unique position configuration.")
    os.exit(1)
  end
end
print("Part 2 - Fewest seconds to display Easter egg:", seconds)
print("Final positions of robots:")
drawGrid(robotsPart2, sizeX, sizeY)
