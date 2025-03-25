
local function find_position(mat, ch)
  for i, row in ipairs(mat) do
    for j = 1, #row do
      if string.sub(row, j, j) == ch then
        return i - 1, j - 1
      end
    end
  end
  return -1, -1
end

local function ok(mat, st, seq)
  local curr_i, curr_j = st[1], st[2]
  for i = 1, #seq do
    local ch = string.sub(seq, i, i)
    if string.sub(mat[curr_i + 1], curr_j + 1, curr_j + 1) == ' ' then
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
    if curr_i < 0 or curr_i >= #mat or curr_j < 0 or curr_j >= #mat[1] then
      return false
    end
  end
  return true
end

local function generate_moves(position, objective, pad)
  local obj_i, obj_j = find_position(pad, objective)
  local pos_i, pos_j = position[1], position[2]
  local ret = ""

  if pos_j > obj_j then
    ret = ret .. string.rep("<", pos_j - obj_j)
  end
  if pos_i > obj_i then
    ret = ret .. string.rep("^", pos_i - obj_i)
  end
  if pos_i < obj_i then
    ret = ret .. string.rep("v", obj_i - pos_i)
  end
  if pos_j < obj_j then
    ret = ret .. string.rep(">", obj_j - pos_j)
  end

  if not ok(pad, {pos_i, pos_j}, ret) then
    ret = ""
    if pos_j < obj_j then
      ret = ret .. string.rep(">", obj_j - pos_j)
    end
    if pos_i > obj_i then
      ret = ret .. string.rep("^", pos_i - obj_i)
    end
    if pos_i < obj_i then
      ret = ret .. string.rep("v", obj_i - pos_i)
    end
    if pos_j > obj_j then
      ret = ret .. string.rep("<", pos_j - obj_j)
    end
  end
  return ret
end

local memo = {}

local function solve(code, robots, key_pad, robot_pad, max_robots)
    if robots <= 0 then
        return #code
    end
    
    local state = code .. ":" .. robots
    if memo[state] then
        return memo[state]
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
    
    memo[state] = ret
    return ret
end

local function main()
  local file = io.open("input.txt", "r")
  local content = file:read("*all")
  io.close(file)

  local max_robots = 3
  local key_pad = {"789", "456", "123", " 0A"}
  local robot_pad = {" ^A", "<v>"}

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
      memo = {}
      local sv = solve(code, max_robots, key_pad, robot_pad, max_robots)
      ret = ret + sv * numeric_part
    end
  end

  print(ret)
end

main()
