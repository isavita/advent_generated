
def solve(input_str)
  blocks = input_str.strip.split("\n\n")
  lines = blocks[0].split("\n")
  m = Hash(Tuple(Int32, Int32), Char).new
  lines.each_with_index do |row, y|
    row.each_char.with_index do |ch, x|
      m[{x, y}] = ch
    end
  end

  steps = [] of Tuple(Int32, Int32)
  blocks[1].each_char do |c|
    case c
    when '^' then steps << {0, -1}
    when '<' then steps << {-1, 0}
    when '>' then steps << {1, 0}
    when 'v' then steps << {0, 1}
    end
  end

  robot = m.find { |_, v| v == '@' }.not_nil![0]

  steps.each do |dir|
    if try_to_step(m, robot, dir)
      robot = {robot[0] + dir[0], robot[1] + dir[1]}
    end
  end

  sum = 0_i64
  m.each do |k, v|
    sum += k[0] + 100 * k[1] if v == '[' || v == 'O'
  end
  sum
end

def try_to_step(m, pos, dir)
  orig = m.dup
  case m[pos]?
  when '.'
    return true
  when 'O', '@'
    if try_to_step(m, {pos[0] + dir[0], pos[1] + dir[1]}, dir)
      m[{pos[0] + dir[0], pos[1] + dir[1]}] = m[pos]
      m[pos] = '.'
      return true
    end
  when ']'
    if try_to_step(m, {pos[0] - 1, pos[1]}, dir)
      return true
    end
  when '['
    if dir == {-1, 0}
      if try_to_step(m, {pos[0] - 1, pos[1]}, dir)
        m[{pos[0] - 1, pos[1]}] = '['
        m[pos] = ']'
        m[{pos[0] + 1, pos[1]}] = '.'
        return true
      end
    elsif dir == {1, 0}
      if try_to_step(m, {pos[0] + 2, pos[1]}, dir)
        m[pos] = '.'
        m[{pos[0] + 1, pos[1]}] = '['
        m[{pos[0] + 2, pos[1]}] = ']'
        return true
      end
    else
      if try_to_step(m, {pos[0] + dir[0], pos[1] + dir[1]}, dir) &&
         try_to_step(m, {pos[0] + 1 + dir[0], pos[1] + dir[1]}, dir)
        m[pos] = '.'
        m[{pos[0] + 1, pos[1]}] = '.'
        m[{pos[0] + dir[0], pos[1] + dir[1]}] = '['
        m[{pos[0] + 1 + dir[0], pos[1] + dir[1]}] = ']'
        return true
      end
    end
  end
  m.clear
  orig.each { |k, v| m[k] = v }
  false
end

def scale_up(input_str)
  s = input_str.gsub("#", "##")
  s = s.gsub(".", "..")
  s = s.gsub("O", "[]")
  s = s.gsub("@", "@.")
  s
end

input = File.read("input.txt")
puts solve(input)
puts solve(scale_up(input))
