
local function read_input(filename)
  local file = io.open(filename, "r")
  if not file then return nil end
  local content = file:read("*all")
  file:close()
  return content
end

local function parse_program(input)
  local program = {}
  for num_str in string.gmatch(input, "([^,]+)") do
    table.insert(program, tonumber(num_str))
  end
  return program
end

local function run_intcode(program, input_func, output_func)
  local memory = {}
  for i, v in ipairs(program) do
    memory[i - 1] = v
  end
  local ip = 0
  local relative_base = 0

  local function get_param(mode, offset)
    local value = memory[ip + offset] or 0
    if mode == 0 then
      return memory[value] or 0
    elseif mode == 1 then
      return value
    elseif mode == 2 then
      return memory[relative_base + value] or 0
    end
  end

  local function set_param(mode, offset, value)
    local addr = memory[ip + offset] or 0
    if mode == 0 then
      memory[addr] = value
    elseif mode == 2 then
      memory[relative_base + addr] = value
    end
  end

  while true do
    local instruction = memory[ip] or 0
    local opcode = instruction % 100
    local mode1 = math.floor(instruction / 100) % 10
    local mode2 = math.floor(instruction / 1000) % 10
    local mode3 = math.floor(instruction / 10000) % 10

    if opcode == 99 then
      break
    elseif opcode == 1 then
      local param1 = get_param(mode1, 1)
      local param2 = get_param(mode2, 2)
      set_param(mode3, 3, param1 + param2)
      ip = ip + 4
    elseif opcode == 2 then
      local param1 = get_param(mode1, 1)
      local param2 = get_param(mode2, 2)
      set_param(mode3, 3, param1 * param2)
      ip = ip + 4
    elseif opcode == 3 then
      local input = input_func()
      set_param(mode1, 1, input)
      ip = ip + 2
    elseif opcode == 4 then
      local output = get_param(mode1, 1)
      output_func(output)
      ip = ip + 2
    elseif opcode == 5 then
      local param1 = get_param(mode1, 1)
      local param2 = get_param(mode2, 2)
      if param1 ~= 0 then
        ip = param2
      else
        ip = ip + 3
      end
    elseif opcode == 6 then
      local param1 = get_param(mode1, 1)
      local param2 = get_param(mode2, 2)
      if param1 == 0 then
        ip = param2
      else
        ip = ip + 3
      end
    elseif opcode == 7 then
      local param1 = get_param(mode1, 1)
      local param2 = get_param(mode2, 2)
      set_param(mode3, 3, param1 < param2 and 1 or 0)
      ip = ip + 4
    elseif opcode == 8 then
      local param1 = get_param(mode1, 1)
      local param2 = get_param(mode2, 2)
      set_param(mode3, 3, param1 == param2 and 1 or 0)
      ip = ip + 4
    elseif opcode == 9 then
      local param1 = get_param(mode1, 1)
      relative_base = relative_base + param1
      ip = ip + 2
    else
      error("Invalid opcode: " .. opcode)
    end
  end
end

local function solve_part1(program)
  local block_count = 0
  local output_values = {}
  local output_index = 1

  local output_func = function(output)
    table.insert(output_values, output)
    if #output_values % 3 == 0 then
      local x = output_values[output_index]
      local y = output_values[output_index + 1]
      local tile_id = output_values[output_index + 2]
      if tile_id == 2 then
        block_count = block_count + 1
      end
      output_index = output_index + 3
    end
  end

  run_intcode(program, function() return 0 end, output_func)
  return block_count
end

local function solve_part2(program)
  program[1] = 2
  local score = 0
  local paddle_x = 0
  local ball_x = 0
  local output_values = {}
  local output_index = 1

  local input_func = function()
    if ball_x < paddle_x then
      return -1
    elseif ball_x > paddle_x then
      return 1
    else
      return 0
    end
  end

  local output_func = function(output)
    table.insert(output_values, output)
    if #output_values % 3 == 0 then
      local x = output_values[output_index]
      local y = output_values[output_index + 1]
      local tile_id = output_values[output_index + 2]
      if x == -1 and y == 0 then
        score = tile_id
      elseif tile_id == 3 then
        paddle_x = x
      elseif tile_id == 4 then
        ball_x = x
      end
      output_index = output_index + 3
    end
  end

  run_intcode(program, input_func, output_func)
  return score
end

local input = read_input("input.txt")
if input then
  local program = parse_program(input)
  local part1_result = solve_part1(program)
  print("Part 1:", part1_result)
  local part2_result = solve_part2(program)
  print("Part 2:", part2_result)
else
  print("Error reading input file.")
end
