
local function parseLevels(line)
  local levels = {}
  for level in string.gmatch(line, "%d+") do
    local num = tonumber(level)
    if not num then return nil end
    table.insert(levels, num)
  end
  return levels
end

local function isSafeReport(levels)
  local n = #levels
  if n < 2 then return false end
  local firstDiff = levels[2] - levels[1]
  if firstDiff == 0 then return false end
  local isIncreasing = firstDiff > 0
  for i = 1, n - 1 do
    local diff = levels[i+1] - levels[i]
    if diff == 0 or (isIncreasing and diff <= 0) or (not isIncreasing and diff >= 0) then return false end
    local absDiff = math.abs(diff)
    if absDiff < 1 or absDiff > 3 then return false end
  end
  return true
end

local file = io.open("input.txt", "r")
if not file then error("Failed to open input file") end

local safeReportCount = 0
for line in file:lines() do
  local levels = parseLevels(line)
  if levels and isSafeReport(levels) then safeReportCount = safeReportCount + 1 end
end

file:close()
print(safeReportCount)
