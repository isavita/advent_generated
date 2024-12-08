
local function solve()
  local file = io.open("input.txt", "r")
  if not file then error("Could not open input.txt") end
  local input = file:read("*all")
  file:close()

  local total = 0
  local pattern = "mul%((%d+),(%d+)%)"
  for num1, num2 in string.gmatch(input, pattern) do
    total = total + tonumber(num1) * tonumber(num2)
  end
  print(total)
end

solve()
