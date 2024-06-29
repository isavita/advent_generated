class Tile
  attr_accessor :kind, :x, :y, :map, :unit

  def initialize(kind, x, y, map)
    @kind = kind
    @x = x
    @y = y
    @map = map
    @unit = nil
  end

  def walkable_neighbors
    [[0, -1], [-1, 0], [1, 0], [0, 1]].map do |dx, dy|
      n = @map.tile(@x + dx, @y + dy)
      n if n && n.kind == :space
    end.compact
  end
end

class Unit
  attr_accessor :kind, :hitpoints, :power, :tile

  def initialize(tile, kind, elf_power)
    @kind = kind
    @hitpoints = 200
    @power = kind == :elf ? elf_power : 3
    @tile = tile
    tile.unit = self
  end

  def enemies(cave)
    cave.units.select { |u| u.kind != @kind && u.hitpoints > 0 }.sort_by { |u| [u.tile.y, u.tile.x] }
  end

  def enemy_neighbor(cave)
    neighbors = [[0, -1], [-1, 0], [1, 0], [0, 1]].map do |dx, dy|
      t = cave.map.tile(@tile.x + dx, @tile.y + dy)
      t&.unit if t && t.unit && t.unit.kind != @kind && t.unit.hitpoints > 0
    end.compact
    neighbors.min_by { |u| [u.hitpoints, u.tile.y, u.tile.x] }
  end

  def move(cave)
    return if enemy_neighbor(cave)
    next_tile, _ = next_tile(cave)
    return unless next_tile
    @tile.kind = :space
    @tile.unit = nil
    next_tile.unit = self
    next_tile.kind = @kind
    @tile = next_tile
  end

  def attack(cave)
    enemy = enemy_neighbor(cave)
    if enemy
      killed = enemy.damage(cave, @power)
      return killed && enemy.kind == :elf
    end
    false
  end

  def damage(cave, damage)
    @hitpoints -= damage
    if @hitpoints <= 0
      cave.remove_unit(self)
      true
    else
      false
    end
  end

  def next_tile(cave)
    targets = []
    closest_target_distance = Float::INFINITY
    distances, path = cave.map.find_walkable_tiles(@tile)
    enemies(cave).each do |enemy|
      enemy.tile.walkable_neighbors.each do |target|
        distance = distances[target]
        next unless distance
        if distance < closest_target_distance
          closest_target_distance = distance
          targets = [target]
        elsif distance == closest_target_distance
          targets << target
        end
      end
    end
    return [nil, nil] if targets.empty?
    target = targets.min_by { |t| [t.y, t.x] }
    current = target
    while path[current] != @tile
      current = path[current]
    end
    [current, target]
  end
end

class Map
  def initialize
    @tiles = {}
  end

  def set_tile(tile, x, y)
    @tiles[y] ||= {}
    @tiles[y][x] = tile
  end

  def tile(x, y)
    @tiles[y] && @tiles[y][x]
  end

  def find_walkable_tiles(start)
    frontier = [start]
    distance = { start => 0 }
    came_from = { start => nil }

    until frontier.empty?
      current = frontier.shift
      current.walkable_neighbors.each do |next_tile|
        unless distance.key?(next_tile)
          frontier << next_tile
          distance[next_tile] = distance[current] + 1
          came_from[next_tile] = current
        end
      end
    end

    [distance, came_from]
  end
end

class Cave
  attr_reader :units, :map

  def initialize(input, elf_power)
    @map = Map.new
    @units = []
    parse_map(input, elf_power)
    @initial_elf_count = @units.count { |u| u.kind == :elf }
  end

  def parse_map(input, elf_power)
    input.each_with_index do |row, y|
      row.chars.each_with_index do |col, x|
        kind = case col
               when '.' then :space
               when 'E' then :elf
               when 'G' then :goblin
               else :wall
               end
        tile = Tile.new(kind, x, y, @map)
        @map.set_tile(tile, x, y)
        @units << Unit.new(tile, kind, elf_power) if [:elf, :goblin].include?(kind)
      end
    end
  end

  def status
    hp = @units.select { |u| u.hitpoints > 0 }.sum(&:hitpoints)
    combat = @units.any? { |u| u.kind == :elf && u.hitpoints > 0 } &&
             @units.any? { |u| u.kind == :goblin && u.hitpoints > 0 }
    [hp, combat]
  end

  def remove_dead
    @units.reject! { |u| u.hitpoints <= 0 }
  end

  def remove_unit(unit)
    unit.tile.kind = :space
    unit.tile.unit = nil
    unit.tile = nil
  end

  def tick(stop_on_elf_death)
    remove_dead
    @units.sort_by! { |u| [u.tile.y, u.tile.x] }

    @units.each do |unit|
      next if unit.hitpoints <= 0
      return [false, false] unless unit.enemies(self).any?
      unit.move(self)
      if unit.attack(self) && stop_on_elf_death
        return [false, true]
      end
    end
    [true, false]
  end

  def all_elves_alive?
    @units.count { |u| u.kind == :elf } == @initial_elf_count
  end
end

def combat(input, elf_power, stop_on_elf_death)
  cave = Cave.new(input, elf_power)
  i = 0
  loop do
    i += 1
    hp, combat = cave.status
    return [(i - 1) * hp, cave.all_elves_alive?] unless combat
    clean_round, elf_died = cave.tick(stop_on_elf_death)
    return [0, false] if elf_died
    i -= 1 unless clean_round
  end
end

def find_minimum_elf_power(input)
  low = 4
  high = 200

  while low < high
    mid = (low + high) / 2
    outcome, elves_won = combat(input, mid, true)
    
    if elves_won
      high = mid
    else
      low = mid + 1
    end
  end

  outcome, _ = combat(input, low, true)
  [low, outcome]
end

input = File.readlines('input.txt', chomp: true)
elf_power, outcome = find_minimum_elf_power(input)
puts "Minimum Elf attack power: #{elf_power}"
puts "Outcome: #{outcome}"
