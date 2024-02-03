
def read_map
  map = {}
  File.readlines("input.txt").each_with_index do |line, y|
    line.strip.each_char.with_index do |char, x|
      map[[x, y]] = char == '#' ? :infected : :clean
    end
  end
  map
end

def burst(map, pos, dir, part2: false)
  turn_right = { up: :right, right: :down, down: :left, left: :up }
  turn_left = turn_right.invert
  reverse = { up: :down, down: :up, left: :right, right: :left }
  move = { up: [0, -1], down: [0, 1], left: [-1, 0], right: [1, 0] }
  infections = 0

  node = map[pos] || :clean
  if part2
    case node
    when :clean
      dir = turn_left[dir]
      map[pos] = :weakened
    when :weakened
      map[pos] = :infected
      infections += 1
    when :infected
      dir = turn_right[dir]
      map[pos] = :flagged
    when :flagged
      dir = reverse[dir]
      map[pos] = :clean
    end
  else
    if node == :clean
      dir = turn_left[dir]
      map[pos] = :infected
      infections += 1
    else
      dir = turn_right[dir]
      map[pos] = :clean
    end
  end
  pos = [pos[0] + move[dir][0], pos[1] + move[dir][1]]
  [pos, dir, infections]
end

def simulate(map, bursts, part2: false)
  pos = [map.keys.map(&:first).sum / map.size, map.keys.map(&:last).sum / map.size]
  dir = :up
  total_infections = 0

  bursts.times do
    pos, dir, infections = burst(map, pos, dir, part2: part2)
    total_infections += infections
  end
  total_infections
end

map = read_map
puts simulate(map.dup, 10_000)
puts simulate(map, 10_000_000, part2: true)
