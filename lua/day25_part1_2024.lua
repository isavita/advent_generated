
local f = io.open("input.txt", "r")
local raw = {}
for line in f:lines() do
  line = line:match("^%s*(.-)%s*$")
  if line ~= "" then
    raw[#raw + 1] = line
  end
end
f:close()
if #raw % 7 ~= 0 then
  print("0")
  return
end
local locks = {}
local keys = {}
local function parseLock(b)
  local h = {}
  for c = 1, 5 do
    local cnt = 0
    for r = 2, 7 do
      if b[r]:sub(c, c) == '#' then
        cnt = cnt + 1
      else
        break
      end
    end
    h[c] = cnt
  end
  return h
end
local function parseKey(b)
  local h = {}
  for c = 1, 5 do
    local cnt = 0
    for r = 6, 1, -1 do
      if b[r]:sub(c, c) == '#' then
        cnt = cnt + 1
      else
        break
      end
    end
    h[c] = cnt
  end
  return h
end
local function fits(lock, key)
  for i = 1, 5 do
    if lock[i] + key[i] > 5 then
      return false
    end
  end
  return true
end
for i = 1, #raw, 7 do
  local block = {}
  for j = 0, 6 do
      block[j+1] = raw[i + j]
  end
  local valid = true
  for _, ln in pairs(block) do
    if #ln < 5 then
      valid = false
      break
    end
  end
  if not valid then
    goto continue
  end
  if block[1]:match("^#+$") then
    locks[#locks + 1] = parseLock(block)
  elseif block[1]:match("^%.+$") then
    keys[#keys + 1] = parseKey(block)
  end
  ::continue::
end
local count = 0
for _, lock in pairs(locks) do
  for _, key in pairs(keys) do
    if fits(lock, key) then
      count = count + 1
    end
  end
end
print(count)
