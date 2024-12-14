
local function calculate(nums, ops)
  local result = nums[1]
  for i = 1, #ops do
    if ops[i] == '+' then
      result = result + nums[i + 1]
    elseif ops[i] == '*' then
      result = result * nums[i + 1]
    end
  end
  return result
end

local function solve_equation(line)
  local test_value, numbers_str = line:match("([^:]+): (.+)")
  test_value = tonumber(test_value)
  local numbers = {}
  for num_str in numbers_str:gmatch("%S+") do
    table.insert(numbers, tonumber(num_str))
  end

  local num_ops = #numbers - 1
  for i = 0, 2^num_ops - 1 do
    local ops = {}
    local temp = i
    for _ = 1, num_ops do
      if temp % 2 == 0 then
        table.insert(ops, '+')
      else
        table.insert(ops, '*')
      end
      temp = math.floor(temp / 2)
    end
    
    if calculate(numbers, ops) == test_value then
      return test_value
    end
  end
  return 0
end

local total_calibration = 0
local file = io.open("input.txt", "r")
if file then
  for line in file:lines() do
    total_calibration = total_calibration + solve_equation(line)
  end
  file:close()
end

print(total_calibration)
