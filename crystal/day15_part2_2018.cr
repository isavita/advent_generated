
class Unit
  property type : Char, x : Int32, y : Int32, hp : Int32, attack_power : Int32, alive : Bool

  def initialize(@type, @x, @y, @attack_power = 3)
    @hp = 200
    @alive = true
  end

  def position
    "#{x},#{y}"
  end
end

DIRECTIONS = [{0, -1}, {-1, 0}, {1, 0}, {0, 1}]

def parse_input(path, elf_attack_power = 3)
  grid = [] of Array(Char)
  units = [] of Unit
  File.read_lines(path).each_with_index do |line, y|
    row = [] of Char
    line.chars.each_with_index do |char, x|
      if char == 'E'
        units << Unit.new(char, x, y, elf_attack_power)
        row << '.'
      elsif char == 'G'
        units << Unit.new(char, x, y, 3)
        row << '.'
      else
        row << char
      end
    end
    grid << row
  end
  {grid, units}
end

def sort_units(units)
  units.sort_by! { |u| {u.y, u.x} }
end

def adjacent(x, y)
  [{x, y - 1}, {x - 1, y}, {x + 1, y}, {x, y + 1}]
end

def in_range_squares(targets, grid, units)
  squares = Set(String).new
  targets.each do |target|
    adjacent(target.x, target.y).each do |pos|
      x, y = pos
      if y >= 0 && y < grid.size && x >= 0 && x < grid[y].size && grid[y][x] == '.' && units.none? { |u| u.alive && u.x == x && u.y == y }
        squares.add("#{x},#{y}")
      end
    end
  end
  squares.map { |s| t = s.split(',').map(&.to_i); {t[0], t[1]} }
end

def bfs(start, targets, grid, units)
  h, w = grid.size, grid[0].size
  visited = Array.new(h) { Array.new(w, false) }
  dist = Array.new(h) { Array.new(w, Int32::MAX) }
  prev = Array.new(h) { Array.new(w, {-1, -1}) }
  queue = Deque({Int32, Int32}).new
  sx, sy = start
  queue << {sx, sy}
  visited[sy][sx] = true
  dist[sy][sx] = 0
  found_dist = Int32::MAX
  reachable = [] of {Int32, Int32}

  while !queue.empty?
    cx, cy = queue.shift
    d = dist[cy][cx]
    next if d > found_dist

    if targets.any? { |t| t[0] == cx && t[1] == cy }
      found_dist = d
      reachable << {cx, cy}
      next
    end

    DIRECTIONS.each do |dx, dy|
      nx, ny = cx + dx, cy + dy
      next unless ny >= 0 && ny < h && nx >= 0 && nx < w
      next if grid[ny][nx] != '.'
      next if units.any? { |u| u.alive && u.x == nx && u.y == ny }
      next if visited[ny][nx]

      visited[ny][nx] = true
      dist[ny][nx] = d + 1
      prev[ny][nx] = {cx, cy}
      queue << {nx, ny}
    end
  end

  return nil if reachable.empty?
  reachable.sort_by! { |t| {t[1], t[0]} }
  chosen = reachable[0]
  cx, cy = chosen
  until prev[cy][cx] == {sx, sy}
    cx, cy = prev[cy][cx]
  end
  {cx, cy}
end

def execute_round(grid, units)
  sort_units(units)
  units.each do |unit|
    next unless unit.alive
    targets = units.select { |u| u.alive && u.type != unit.type }
    return false if targets.empty?

    adjacent_enemies = adjacent(unit.x, unit.y).select do |x, y|
      units.any? { |u| u.alive && u.type != unit.type && u.x == x && u.y == y }
    end

    if adjacent_enemies.empty?
      in_range = in_range_squares(targets, grid, units)
      next if in_range.empty?
      step = bfs({unit.x, unit.y}, in_range, grid, units)
      if step
        unit.x, unit.y = step
        if adjacent(unit.x, unit.y).any? { |x, y| units.any? { |u| u.alive && u.type != unit.type && u.x == x && u.y == y } }
          attack(unit, units)
        end
      end
    else
      attack(unit, units)
    end
  end
  true
end

def attack(unit, units)
  enemies = adjacent(unit.x, unit.y).compact_map do |x, y|
    units.find { |u| u.alive && u.type != unit.type && u.x == x && u.y == y }
  end
  return if enemies.empty?
  target = enemies.min_by { |e| {e.hp, e.y, e.x} }
  target.hp -= unit.attack_power
  target.alive = false if target.hp <= 0
end

def outcome(rounds, units)
  total_hp = units.select(&.alive).sum(&.hp)
  rounds * total_hp
end

def simulate(path, elf_power)
  grid, units = parse_input(path, elf_power)
  initial_elves = units.count { |u| u.type == 'E' }
  rounds = 0
  loop do
    complete = execute_round(grid, units)
    alive_elves = units.count { |u| u.type == 'E' && u.alive }
    return {false, true, 0} if alive_elves < initial_elves
    break unless complete
    rounds += 1
  end
  goblins_alive = units.any? { |u| u.type == 'G' && u.alive }
  elves_alive = units.count { |u| u.type == 'E' && u.alive }
  result = outcome(rounds, units)
  elves_won = elves_alive > 0 && !goblins_alive
  elves_died = elves_alive < initial_elves
  {elves_won, elves_died, result}
end

elf_power = 4
loop do
  won, died, score = simulate("input.txt", elf_power)
  if won && !died
    puts score
    break
  end
  elf_power += 1
end
