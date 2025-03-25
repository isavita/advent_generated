
local function find_position(mat, ch)
  for i = 1, #mat do
    for j = 1, #mat[i] do
      if string.sub(mat[i], j, j) == ch then
        return i - 1, j - 1
      end
    end
  end
  return -1, -1
end

local function ok(mat, st, seq)
  local curr_i, curr_j = st[1], st[2]
  for k = 1, #seq do
    local ch = string.sub(seq, k, k)
    if not (curr_i >= 0 and curr_i < #mat and curr_j >= 0 and curr_j < #mat[curr_i+1]) or string.sub(mat[curr_i+1], curr_j+1, curr_j+1) == ' ' then
      return false
    end
    if ch == '^' then
      curr_i = curr_i - 1
    elseif ch == 'v' then
      curr_i = curr_i + 1
    elseif ch == '<' then
      curr_j = curr_j - 1
    elseif ch == '>' then
      curr_j = curr_j + 1
    end
  end
  return true
end

local function generate_moves(position, objective, pad)
  local obj_pos_i, obj_pos_j = find_position(pad, objective)
  local pos_i, pos_j = position[1], position[2]

  local result = ""
  if pos_j > obj_pos_j then
    result = result .. string.rep("<", pos_j - obj_pos_j)
  end
  if pos_i > obj_pos_i then
    result = result .. string.rep("^", pos_i - obj_pos_i)
  end
  if pos_i < obj_pos_i then
    result = result .. string.rep("v", obj_pos_i - pos_i)
  end
  if pos_j < obj_pos_j then
    result = result .. string.rep(">", obj_pos_j - pos_j)
  end

  if not ok(pad, {pos_i, pos_j}, result) then
    result = ""
    if pos_j < obj_pos_j then
      result = result .. string.rep(">", obj_pos_j - pos_j)
    end
    if pos_i > obj_pos_i then
      result = result .. string.rep("^", pos_i - obj_pos_i)
    end
    if pos_i < obj_pos_i then
      result = result .. string.rep("v", obj_pos_i - pos_i)
    end
    if pos_j > obj_pos_j then
      result = result .. string.rep("<", pos_j - obj_pos_j)
    end
  end

  return result
end

local memo = {}

local function solve(code, robots, key_pad, robot_pad, max_robots)
  local key = code .. ":" .. robots .. ":" .. max_robots
  if memo[key] then
    return memo[key]
  end

  if robots <= 0 then
    return #code
  end

  local ret = 0
  local pos_i, pos_j = 3, 2
  if robots ~= max_robots then
    pos_i = 0
  end

  for i = 1, #code do
    local ch = string.sub(code, i, i)
    local moves
    if robots == max_robots then
      moves = generate_moves({pos_i, pos_j}, ch, key_pad)
      pos_i, pos_j = find_position(key_pad, ch)
    else
      moves = generate_moves({pos_i, pos_j}, ch, robot_pad)
      pos_i, pos_j = find_position(robot_pad, ch)
    end
    ret = ret + solve(moves .. "A", robots - 1, key_pad, robot_pad, max_robots)
  end

  memo[key] = ret
  return ret
end

local function main()
  local file = io.open("input.txt", "r")
  local content = file:read("*a")
  io.close(file)

  local max_robots = 26
  local key_pad = {
    "789",
    "456",
    "123",
    " 0A",
  }
  local robot_pad = {
    " ^A",
    "<v>",
  }

  local ret = 0
  for code in string.gmatch(content, "[^\r\n]+") do
    code = string.gsub(code, "^%s*(.-)%s*$", "%1")
    if code ~= "" then
      local numeric_part = 0
      for i = 1, #code do
        local char = string.sub(code, i, i)
        if char >= '0' and char <= '9' then
          numeric_part = numeric_part * 10 + (char - '0')
        end
      end
      ret = ret + solve(code, max_robots, key_pad, robot_pad, max_robots) * numeric_part
    end
  end

  print(ret)
end

main()
