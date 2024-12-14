
local function split_number(num)
  local str_num = tostring(num)
  local len = #str_num
  if len % 2 ~= 0 then
    return nil
  end
  local mid = len / 2
  return tonumber(str_num:sub(1, mid)), tonumber(str_num:sub(mid + 1))
end

local function transform_stones(stones)
  local new_stones = {}
  for _, stone in ipairs(stones) do
    if stone == 0 then
      table.insert(new_stones, 1)
    elseif #tostring(stone) % 2 == 0 then
      local left, right = split_number(stone)
      table.insert(new_stones, left)
      table.insert(new_stones, right)
    else
      table.insert(new_stones, stone * 2024)
    end
  end
  return new_stones
end

local function solve()
  local file = io.open("input.txt", "r")
  if not file then
    print("Error: input.txt not found")
    return
  end
  local line = file:read("*l")
  file:close()

  local stones = {}
  for num_str in string.gmatch(line, "%d+") do
    table.insert(stones, tonumber(num_str))
  end

  for _ = 1, 25 do
    stones = transform_stones(stones)
  end

  print(#stones)
end

solve()
