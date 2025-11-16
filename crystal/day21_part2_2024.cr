
require "json"

class KeypadSolver
  @position_cache = {} of String => Tuple(Int32, Int32)
  @ok_cache = {} of String => Bool
  @move_cache = {} of String => String
  @solve_cache = {} of String => Int64

  def initialize
    @keypad = ["789", "456", "123", " 0A"]
    @robotpad = [" ^A", "<v>"]
  end

  def find_position(mat, ch)
    key = "#{ch}#{mat.join}"
    return @position_cache[key] if @position_cache.has_key?(key)
    mat.each_with_index do |row, i|
      j = row.index(ch)
      if j
        pos = {i, j}
        @position_cache[key] = pos
        return pos
      end
    end
    {-1, -1}
  end

  def ok(mat, st, seq)
    key = "#{st[0]},#{st[1]},#{seq},#{mat.join}"
    return @ok_cache[key] if @ok_cache.has_key?(key)
    curr = {st[0], st[1]}
    seq.each_char do |ch|
      return (@ok_cache[key] = false) if mat[curr[0]][curr[1]] == ' '
      case ch
      when '^'
        curr = {curr[0] - 1, curr[1]}
      when 'v'
        curr = {curr[0] + 1, curr[1]}
      when '<'
        curr = {curr[0], curr[1] - 1}
      when '>'
        curr = {curr[0], curr[1] + 1}
      end
      return (@ok_cache[key] = false) if curr[0] < 0 || curr[0] >= mat.size || curr[1] < 0 || curr[1] >= mat[0].size
    end
    @ok_cache[key] = true
  end

  def generate_moves(position, objective, pad)
    key = "#{position}#{objective}#{pad.join}"
    return @move_cache[key] if @move_cache.has_key?(key)
    obj_pos = find_position(pad, objective)
    ret = ""
    if position[1] > obj_pos[1]
      ret += "<" * (position[1] - obj_pos[1])
    end
    if position[0] > obj_pos[0]
      ret += "^" * (position[0] - obj_pos[0])
    end
    if position[0] < obj_pos[0]
      ret += "v" * (obj_pos[0] - position[0])
    end
    if position[1] < obj_pos[1]
      ret += ">" * (obj_pos[1] - position[1])
    end
    unless ok(pad, position, ret)
      ret = ""
      if position[1] < obj_pos[1]
        ret += ">" * (obj_pos[1] - position[1])
      end
      if position[0] > obj_pos[0]
        ret += "^" * (position[0] - obj_pos[0])
      end
      if position[0] < obj_pos[0]
        ret += "v" * (obj_pos[0] - position[0])
      end
      if position[1] > obj_pos[1]
        ret += "<" * (position[1] - obj_pos[1])
      end
    end
    @move_cache[key] = ret
  end

  def solve(code, robots, max_robots)
    key = "#{code}#{robots}#{max_robots}"
    return @solve_cache[key] if @solve_cache.has_key?(key)
    if robots <= 0
      return code.size.to_i64
    end
    ret = 0_i64
    posi = robots == max_robots ? 3 : 0
    posj = robots == max_robots ? 2 : 2
    code.each_char do |ch|
      if robots == max_robots
        moves = generate_moves({posi, posj}, ch, @keypad)
        pos = find_position(@keypad, ch)
        posi, posj = pos
      else
        moves = generate_moves({posi, posj}, ch, @robotpad)
        pos = find_position(@robotpad, ch)
        posi, posj = pos
      end
      ret += solve(moves + "A", robots - 1, max_robots)
    end
    @solve_cache[key] = ret
  end
end

solver = KeypadSolver.new
content = File.read("input.txt")
max_robots = 26
ret = 0_i64
codes = content.lines.map(&.strip).select(&.empty?.!)

codes.each do |code|
  numeric_part = code.chars.select(&.number?).join.to_i64
  sv = solver.solve(code, max_robots, max_robots)
  ret += sv * numeric_part
end

puts ret
