
#!/usr/bin/env crystal

def find_position(mat : Array(String), ch : Char) : {Int32, Int32}
  mat.each_with_index do |row, i|
    row.each_char.with_index do |c, j|
      return {i, j} if c == ch
    end
  end
  {-1, -1}
end

def ok?(mat : Array(String), st : {Int32, Int32}, seq : String) : Bool
  i, j = st
  seq.each_char do |c|
    return false if mat[i][j] == ' '
    case c
    when '^' then i -= 1
    when 'v' then i += 1
    when '<' then j -= 1
    when '>' then j += 1
    end
    return false if i < 0 || i >= mat.size || j < 0 || j >= mat[0].size
  end
  true
end

def generate_moves(position : {Int32, Int32}, objective : Char, pad : Array(String)) : String
  obj_i, obj_j = find_position(pad, objective)
  pos_i, pos_j = position
  moves = ""

  if pos_j > obj_j
    moves += "<" * (pos_j - obj_j)
  end
  if pos_i > obj_i
    moves += "^" * (pos_i - obj_i)
  end
  if pos_i < obj_i
    moves += "v" * (obj_i - pos_i)
  end
  if pos_j < obj_j
    moves += ">" * (obj_j - pos_j)
  end

  unless ok?(pad, position, moves)
    moves = ""
    if pos_j < obj_j
      moves += ">" * (obj_j - pos_j)
    end
    if pos_i > obj_i
      moves += "^" * (pos_i - obj_i)
    end
    if pos_i < obj_i
      moves += "v" * (obj_i - pos_i)
    end
    if pos_j > obj_j
      moves += "<" * (pos_j - obj_j)
    end
  end
  moves
end

def solve(code : String, robots : Int32, key_pad : Array(String), robot_pad : Array(String), max_robots : Int32, memo : Hash(String, Int64)) : Int64
  return code.size.to_i64 if robots <= 0

  key = "#{code}|#{robots}"
  return memo[key] if memo.has_key?(key)

  ret = 0_i64
  pos_i = 3_i32
  pos_j = 2_i32
  pos_i = 0_i32 if robots != max_robots

  code.each_char do |ch|
    if robots == max_robots
      moves = generate_moves({pos_i, pos_j}, ch, key_pad)
      pos_i, pos_j = find_position(key_pad, ch)
    else
      moves = generate_moves({pos_i, pos_j}, ch, robot_pad)
      pos_i, pos_j = find_position(robot_pad, ch)
    end
    ret += solve(moves + "A", robots - 1, key_pad, robot_pad, max_robots, memo)
  end

  memo[key] = ret
  ret
end

# Main
content = File.read("input.txt")
max_robots = 3_i32
key_pad = ["789", "456", "123", " 0A"]
robot_pad = [" ^A", "<v>"]

total = 0_i64

content.split("\n").each do |code|
  code = code.strip
  next if code.empty?

  numeric_part = 0_i64
  code.each_char do |c|
    if '0' <= c <= '9'
      numeric_part = numeric_part * 10 + (c - '0')
    end
  end

  memo = Hash(String, Int64).new
  sv = solve(code, max_robots, key_pad, robot_pad, max_robots, memo)
  total += sv * numeric_part
end

puts total
