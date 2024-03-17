local function readAll(path)
  local file = io.open(path, "r")
  if not file then
    error("Failed to open file: " .. path)
  end
  local content = file:read("*a")
  file:close()
  return content:gsub("^%s*(.-)%s*$", "%1")
end

local function toInt(s)
  return tonumber(s)
end

local function mix(nums)
  local n = #nums
  for i = 1, #nums do
    local oldpos = nums[i].pos
    local newpos = ((oldpos + nums[i].val) % (n - 1) + (n - 1)) % (n - 1)
    if oldpos < newpos then
      for j = 1, #nums do
        if nums[j].pos > oldpos and nums[j].pos <= newpos then
          nums[j].pos = nums[j].pos - 1
        end
      end
    elseif newpos < oldpos then
      for j = 1, #nums do
        if nums[j].pos >= newpos and nums[j].pos < oldpos then
          nums[j].pos = nums[j].pos + 1
        end
      end
    end
    nums[i].pos = newpos
  end
end

local function coords(nums)
  local l = #nums
  local zeroPos
  for i = 1, #nums do
    if nums[i].val == 0 then
      zeroPos = nums[i].pos
      break
    end
  end
  local sum = 0
  for i = 1, #nums do
    if nums[i].pos == (zeroPos + 1000) % l or nums[i].pos == (zeroPos + 2000) % l or nums[i].pos == (zeroPos + 3000) % l then
      sum = sum + nums[i].val
    end
  end
  return sum
end

local function main()
  local nums = {}
  for line in readAll("input.txt"):gmatch("[^\r\n]+") do
    table.insert(nums, {pos = #nums, val = toInt(line)})
  end
  local nums2 = {}
  for i = 1, #nums do
    table.insert(nums2, {pos = nums[i].pos, val = 811589153 * nums[i].val})
  end

  for i = 1, 10 do
    mix(nums2)
  end
  print(coords(nums2))
end

main()