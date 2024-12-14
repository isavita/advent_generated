
local function trimLeadingZeros(s)
  local i = 1
  while i < #s and s:sub(i, i) == '0' do
    i = i + 1
  end
  return s:sub(i)
end

local function splitStone(s)
  local mid = math.floor(#s / 2)
  local left = trimLeadingZeros(s:sub(1, mid))
  local right = trimLeadingZeros(s:sub(mid + 1))
  if left == "" then
    left = "0"
  end
  if right == "" then
    right = "0"
  end
  return left, right
end

local function multiplyBy2024(s)
  local num = {}
  for i = 1, #s do
    num[i] = tonumber(s:sub(i, i))
  end
  local multiplier = {2, 0, 2, 4}
  local result = {}
  for i = 1, #num + #multiplier do
    result[i] = 0
  end
  for i = #num, 1, -1 do
    local carry = 0
    for j = #multiplier, 1, -1 do
      local product = (num[i] * multiplier[j]) + result[i + j] + carry
      result[i + j] = product % 10
      carry = math.floor(product / 10)
    end
    result[i] = result[i] + carry
  end
  local start = 1
  while start < #result and result[start] == 0 do
    start = start + 1
  end
  local sb = ""
  for i = start, #result do
    sb = sb .. result[i]
  end
  return sb
end

local file = io.open("input.txt", "r")
if not file then
  print("Error opening input.txt")
  return
end
local line = file:read("*l")
file:close()
if not line then
  print("Input file is empty")
  return
end
local stonesStr = {}
for s in line:gmatch("%S+") do
  table.insert(stonesStr, s)
end

local stonesMap = {}
for _, s in ipairs(stonesStr) do
  stonesMap[s] = (stonesMap[s] or 0) + 1
end

local steps = 75
for step = 1, steps do
  local newStonesMap = {}
  for stone, count in pairs(stonesMap) do
    if stone == "0" then
      newStonesMap["1"] = (newStonesMap["1"] or 0) + count
    elseif #stone % 2 == 0 then
      local left, right = splitStone(stone)
      newStonesMap[left] = (newStonesMap[left] or 0) + count
      newStonesMap[right] = (newStonesMap[right] or 0) + count
    else
      local newStone = multiplyBy2024(stone)
      newStonesMap[newStone] = (newStonesMap[newStone] or 0) + count
    end
  end
  stonesMap = newStonesMap
end

local totalStones = 0
for _, count in pairs(stonesMap) do
  totalStones = totalStones + count
end

print(totalStones)
