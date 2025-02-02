
class Position
  attr_accessor :i, :j

  def initialize(i, j)
    @i = i
    @j = j
  end
end

def find_position(mat, ch)
  mat.each_with_index do |row, i|
    row.each_char.with_index do |cell, j|
      return Position.new(i, j) if cell == ch
    end
  end
  Position.new(-1, -1)
end

def ok?(mat, st, seq)
  curr = st.dup
  seq.each_char do |ch|
    return false if mat[curr.i][curr.j] == ' '
    case ch
    when '^'
      curr.i -= 1
    when 'v'
      curr.i += 1
    when '<'
      curr.j -= 1
    when '>'
      curr.j += 1
    end
    return false if curr.i < 0 || curr.i >= mat.size || curr.j < 0 || curr.j >= mat[0].size
  end
  true
end

def generate_moves(position, objective, pad)
  obj_pos = find_position(pad, objective)
  ret = ''
  if position.j > obj_pos.j
    ret += '<' * (position.j - obj_pos.j)
  end
  if position.i > obj_pos.i
    ret += '^' * (position.i - obj_pos.i)
  end
  if position.i < obj_pos.i
    ret += 'v' * (obj_pos.i - position.i)
  end
  if position.j < obj_pos.j
    ret += '>' * (obj_pos.j - position.j)
  end

  if !ok?(pad, position, ret)
    ret = ''
    if position.j < obj_pos.j
      ret += '>' * (obj_pos.j - position.j)
    end
    if position.i > obj_pos.i
      ret += '^' * (position.i - obj_pos.i)
    end
    if position.i < obj_pos.i
      ret += 'v' * (obj_pos.i - position.i)
    end
    if position.j > obj_pos.j
      ret += '<' * (position.j - obj_pos.j)
    end
  end
  ret
end

def solve(code, robots, key_pad, robot_pad, max_robots)
  return code.size if robots <= 0

  ret = 0
  posi, posj = 3, 2
  posi = 0 if robots != max_robots

  code.each_char do |ch|
    moves = if robots == max_robots
              generate_moves(Position.new(posi, posj), ch, key_pad)
            else
              generate_moves(Position.new(posi, posj), ch, robot_pad)
            end
    pos = find_position(robots == max_robots ? key_pad : robot_pad, ch)
    posi, posj = pos.i, pos.j
    ret += solve(moves + 'A', robots - 1, key_pad, robot_pad, max_robots)
  end
  ret
end

content = File.read('input.txt')
max_robots = 3
key_pad = ["789", "456", "123", " 0A"]
robot_pad = [" ^A", "<v>"]

ret = 0
codes = content.split("\n").map(&:strip).reject(&:empty?)

codes.each do |code|
  numeric_part = code.scan(/\d+/).join.to_i
  sv = solve(code, max_robots, key_pad, robot_pad, max_robots)
  ret += sv * numeric_part
end

puts ret
