
local function parse_line(line)
  local type1, val1, type2, val2_start, val2_end = line:match("(%w)=(%d+), (%w)=(%d+)%.%.(%d+)")
  val1 = tonumber(val1)
  val2_start = tonumber(val2_start)
  val2_end = tonumber(val2_end)

  local coords = {}
  if type1 == "x" then
    local x = val1
    for y = val2_start, val2_end do
      table.insert(coords, {x = x, y = y})
    end
  else -- type1 == "y"
    local y = val1
    for x = val2_start, val2_end do
      table.insert(coords, {x = x, y = y})
    end
  end
  return coords
end

local function main()
  local file = io.open("input.txt", "r")
  if not file then
    error("Could not open input.txt")
  end
  local input_str = file:read("*a")
  file:close()
  input_str = input_str:match("^%s*(.-)%s*$")

  local clay_coords = {}
  local minX, maxX, minY, maxY = 500, 500, math.huge, 0

  for line in input_str:gmatch("[^\n]+") do
    local coords = parse_line(line)
    for _, c in ipairs(coords) do
      table.insert(clay_coords, c)
      minX = math.min(minX, c.x)
      maxX = math.max(maxX, c.x)
      minY = math.min(minY, c.y)
      maxY = math.max(maxY, c.y)
    end
  end

  minX = minX - 1
  maxX = maxX + 1

  local width = maxX - minX + 1
  local height = maxY + 1
  local ground = {}
  for y = 0, height do
    ground[y] = {}
    for x = 1, width do
      ground[y][x] = '.'
    end
  end

  for _, c in ipairs(clay_coords) do
    local gy = c.y
    local gx = c.x - minX + 1
    if gy >= 0 and gy <= height and gx >= 1 and gx <= width then
       ground[gy][gx] = '#'
    end
  end

  local water_count = 0
  local flow_count = 0
  local source_x = 500 - minX + 1

  local stack = {{x = source_x, y = 0}}

  while #stack > 0 do
    local current = table.remove(stack)
    local x, y = current.x, current.y

    while y + 1 <= maxY do
      local below = ground[y+1][x]
      if below == '.' then
        y = y + 1
        ground[y][x] = '|'
      elseif below == '#' or below == '~' then
        break
      else -- below == '|'
         goto continue_loop -- Already flowing here or below is boundary
      end
    end

     if y + 1 > maxY then
       goto continue_loop
     end

    -- Hit clay or settled water, try to spread
    while true do
      local lx, rx = x, x
      local blocked_left, blocked_right = false, false

      -- Scan left
      while true do
          if ground[y][lx-1] == '#' then
              blocked_left = true
              break
          end
          ground[y][lx-1] = '|'
          if ground[y+1][lx-1] == '.' or ground[y+1][lx-1] == '|' then
              table.insert(stack, {x = lx-1, y = y})
              break
          end
          lx = lx - 1
      end

      -- Scan right
      while true do
          if ground[y][rx+1] == '#' then
              blocked_right = true
              break
          end
          ground[y][rx+1] = '|'
          if ground[y+1][rx+1] == '.' or ground[y+1][rx+1] == '|' then
              table.insert(stack, {x = rx+1, y = y})
              break
          end
          rx = rx + 1
      end

      local fill_char = '|'
      if blocked_left and blocked_right then
          fill_char = '~'
      end

      for fx = lx, rx do
          ground[y][fx] = fill_char
      end

      if fill_char == '~' then
          y = y - 1 -- Move up to check the layer above
          if y < 0 then break end -- Avoid infinite loop if source is contained
      else
          break -- Water flowed down left or right
      end

    end
    ::continue_loop::
  end

  for y = minY, maxY do
    for x = 1, width do
      if ground[y][x] == '~' then
        water_count = water_count + 1
      elseif ground[y][x] == '|' then
        flow_count = flow_count + 1
      end
    end
  end

  print(flow_count + water_count)
end

main()
