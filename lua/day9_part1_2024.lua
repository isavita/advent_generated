
local f = io.open("input.txt", "r")
local line = f:read("*line"):gsub("%s", "")
f:close()

local disk = {}
local fileID = 0
local isFile = true
for i = 1, #line do
  local length = tonumber(line:sub(i, i))
  if isFile then
    for j = 1, length do
      table.insert(disk, fileID)
    end
    fileID = fileID + 1
  else
    for j = 1, length do
      table.insert(disk, -1)
    end
  end
  isFile = not isFile
end

while true do
  local lfree = -1
  for i, v in ipairs(disk) do
    if v == -1 then
      lfree = i
      break
    end
  end
  if lfree == -1 then
    break
  end
  local rfile = -1
  for i = #disk, lfree + 1, -1 do
    if disk[i] ~= -1 then
      rfile = i
      break
    end
  end
  if rfile == -1 then
    break
  end
  disk[lfree] = disk[rfile]
  disk[rfile] = -1
end

local checksum = 0
for i, v in ipairs(disk) do
  if v ~= -1 then
    checksum = checksum + (i - 1) * v
  end
end
print(checksum)
