
local file = io.open("input.txt", "r")
if not file then
  error("Failed to open input.txt")
end

local left = {}
local right = {}
local lineNumber = 0

for line in file:lines() do
  lineNumber = lineNumber + 1
  local fields = {}
  for str in string.gmatch(line, "%S+") do
    table.insert(fields, str)
  end
  if #fields ~= 2 then
    error(string.format("Invalid input format at line %d: expected 2 numbers, got %d", lineNumber, #fields))
  end

  local leftNum, err = tonumber(fields[1])
  if not leftNum then
    error(string.format("Invalid number in left list at line %d: %s", lineNumber, err))
  end
  local rightNum, err = tonumber(fields[2])
  if not rightNum then
    error(string.format("Invalid number in right list at line %d: %s", lineNumber, err))
  end

  table.insert(left, leftNum)
  table.insert(right, rightNum)
end

file:close()

local rightCount = {}
for _, num in ipairs(right) do
  rightCount[num] = (rightCount[num] or 0) + 1
end

local similarityScore = 0
for _, num in ipairs(left) do
  similarityScore = similarityScore + (rightCount[num] or 0) * num
end

print(similarityScore)
