
N, E, S, W = 0, 1, 2, 3
DIRS = [[-1, 0], [0, 1], [1, 0], [0, -1]]

def solve
  map, movements, size = parse_input
  human = { curr: [0, size], facing: E }

  movements.each do |mov|
    human[:facing] = rotate(human[:facing], mov[:rotate]) if mov[:rotate]
    mov[:steps].times { break unless walk(human, map, size) } if mov[:steps]
  end

  puts 1000 * (human[:curr][0] + 1) + 4 * (human[:curr][1] + 1) + (human[:facing] + 3) % 4
end

def parse_input
  lines = File.readlines("input.txt", chomp: true)
  size = lines[0].length / 3
  map = {}
  lines.each_with_index do |line, r|
    break if line.empty?
    line.chars.each_with_index do |char, c|
      map[[r, c]] = char == '#' if char != ' '
    end
  end
  movements = parse_path(lines[lines.find_index(&:empty?) + 1])
  [map, movements, size]
end

def parse_path(path)
  movements = []
  acc = 0
  path.chars.each do |char|
    case char
    when 'R'
      movements << { steps: acc } if acc > 0
      acc = 0
      movements << { rotate: 'R' }
    when 'L'
      movements << { steps: acc } if acc > 0
      acc = 0
      movements << { rotate: 'L' }
    else
      acc = 10 * acc + char.to_i
    end
  end
  movements << { steps: acc } if acc > 0
  movements
end

def rotate(dir, direction)
  case direction
  when 'R'
    (dir + 1) % 4
  when 'L'
    (dir - 1 + 4) % 4
  end
end

def walk(human, map, size)
  dir_delta = DIRS[human[:facing]]
  next_pos = [human[:curr][0] + dir_delta[0], human[:curr][1] + dir_delta[1]]
  if map.key?(next_pos)
    return false if map[next_pos]
    human[:curr] = next_pos
    return true
  end

  next_pos, next_facing = cross_border(next_pos, human[:facing], size)
  return false if map[next_pos]
  human[:curr] = next_pos
  human[:facing] = next_facing
  true
end

def cross_border(pos, dir, size)
  x, y = pos
  case
  when x == -1 && y < 2 * size
    [[y + 2 * size, x + 1], E]
  when x == -1 && y >= 2 * size
    [[x + 4 * size, y - 2 * size], N]
  when x == size && dir == S
    [[y - size, x + size - 1], W]
  when x == 2 * size - 1 && dir == N
    [[y + size, x - size + 1], E]
  when x == 3 * size && dir == S
    [[y + 2 * size, x - 2 * size - 1], W]
  when x == 4 * size
    [[x - 4 * size, y + 2 * size], S]
  when y == -1 && x < 3 * size
    [[3 * size - 1 - x, y + size + 1], E]
  when y == -1 && x >= 3 * size
    [[y + 1, x - 2 * size], S]
  when y == size - 1 && x < size
    [[3 * size - 1 - x, y - size + 1], E]
  when y == size - 1 && x >= size && dir == W
    [[y + size + 1, x - size], S]
  when y == size && dir == E
    [[y + 2 * size - 1, x - 2 * size], N]
  when y == 2 * size && x < 2 * size && dir == E
    [[y - size - 1, x + size], N]
  when y == 2 * size && x >= 2 * size
    [[3 * size - 1 - x, y + size - 1], W]
  when y == 3 * size
    [[3 * size - 1 - x, y - size - 1], W]
  else
    raise "not a border crossing"
  end
end

solve
