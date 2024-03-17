local function read_all(path)
  local file = io.open(path, "r")
  if not file then
    error("Error opening file: " .. path)
  end
  local content = file:read("*a")
  file:close()
  return content
end

local function abs(x)
  return x < 0 and -x or x
end

local x = {1}
for line in string.gmatch(read_all("input.txt"), "[^\n]+") do
  if line == "noop" then
    table.insert(x, x[#x])
  elseif string.sub(line, 1, 4) == "addx" then
    local n = tonumber(string.sub(line, 6))
    table.insert(x, x[#x])
    table.insert(x, x[#x] + n)
  end
end

local sum = 0
for i = 1, #x do
  if (i - 20) % 40 == 0 then
    sum = sum + i * x[i]
  end
end

print(sum)