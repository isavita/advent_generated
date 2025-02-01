
def find_position(mat, ch)
  @position_cache ||= {}
  key = "#{ch}#{mat.join}"
  return @position_cache[key] if @position_cache.key?(key)

  mat.each_with_index do |row, i|
    row.each_char.with_index do |c, j|
      if c == ch
        @position_cache[key] = [i, j]
        return [i, j]
      end
    end
  end
  [-1, -1]
end

def ok(mat, st, seq)
  @ok_cache ||= {}
  key = "#{st[0]},#{st[1]},#{seq},#{mat.join}"
  return @ok_cache[key] if @ok_cache.key?(key)

  curr = st.dup
  seq.each_char do |ch|
    return @ok_cache[key] = false if mat[curr[0]][curr[1]] == ' '

    case ch
    when '^'
      curr[0] -= 1
    when 'v'
      curr[0] += 1
    when '<'
      curr[1] -= 1
    when '>'
      curr[1] += 1
    end
    
    return @ok_cache[key] = false if curr[0] < 0 || curr[0] >= mat.length || curr[1] < 0 || curr[1] >= mat[0].length
  end

  @ok_cache[key] = true
end

def generate_moves(position, objective, pad)
  @move_cache ||= {}
  key = [position, objective, pad.join]
  return @move_cache[key] if @move_cache.key?(key)
  
  obj_pos = find_position(pad, objective)
  moves = ""

    
  if position[1] > obj_pos[1]
      moves += '<' * (position[1] - obj_pos[1])
  end
  if position[0] > obj_pos[0]
    moves += '^' * (position[0] - obj_pos[0])
  end
  if position[0] < obj_pos[0]
      moves += 'v' * (obj_pos[0] - position[0])
  end
    if position[1] < obj_pos[1]
      moves += '>' * (obj_pos[1] - position[1])
    end

  
    if !ok(pad, position, moves)
        moves = ""
        if position[1] < obj_pos[1]
            moves += '>' * (obj_pos[1] - position[1])
        end
        if position[0] > obj_pos[0]
          moves += '^' * (position[0] - obj_pos[0])
        end
        if position[0] < obj_pos[0]
            moves += 'v' * (obj_pos[0] - position[0])
        end
        if position[1] > obj_pos[1]
            moves += '<' * (position[1] - obj_pos[1])
        end
    end
  @move_cache[key] = moves
  moves
end


def solve(code, robots, key_pad, robot_pad, max_robots)
    @solve_cache ||= {}
    key = [code, robots, max_robots]
    return @solve_cache[key] if @solve_cache.key?(key)

  return code.length if robots <= 0

  ret = 0
  posi, posj = 3, 2
  posi = 0 if robots != max_robots

  code.each_char do |ch|
    moves = if robots == max_robots
              generate_moves([posi, posj], ch, key_pad)
            else
              generate_moves([posi, posj], ch, robot_pad)
            end
    pos = if robots == max_robots
            find_position(key_pad, ch)
          else
            find_position(robot_pad, ch)
          end
    posi, posj = pos[0], pos[1]
    ret += solve(moves + "A", robots - 1, key_pad, robot_pad, max_robots)
  end

  @solve_cache[key] = ret
end

content = File.read("input.txt").strip
max_robots = 26
key_pad = [
  "789",
  "456",
  "123",
  " 0A"
]
robot_pad = [
  " ^A",
  "<v>",
]

total_sum = 0

content.split("\n").each do |code|
    code.strip!
    next if code.empty?

  numeric_part = 0
  code.each_char do |char|
    if char >= '0' && char <= '9'
      numeric_part = numeric_part * 10 + (char.to_i)
    end
  end
  total_sum += solve(code, max_robots, key_pad, robot_pad, max_robots) * numeric_part
end
puts total_sum
