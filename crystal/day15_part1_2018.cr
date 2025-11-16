
class Tile
  @kind : Symbol
  @x : Int32
  @y : Int32
  @unit : Unit?

  def initialize(@kind, @x, @y, @unit = nil)
  end

  def walkable_neighbors(game_map : Array(Array(Tile)))
    neighbors = [] of Tile
    max_y = game_map.size
    return neighbors if max_y == 0
    max_x = game_map[0].size
    [{0, -1}, {-1, 0}, {1, 0}, {0, 1}].each do |(dx, dy)|
      nx = @x + dx
      ny = @y + dy
      if ny >= 0 && ny < max_y && nx >= 0 && nx < max_x
        n_tile = game_map[ny][nx]
        if n_tile && n_tile.kind == :space
          neighbors << n_tile
        end
      end
    end
    neighbors
  end

  property :kind, :x, :y, :unit
end

class Unit
  DEFAULT_HITPOINTS = 200
  DEFAULT_POWER   = 3

  @kind : Symbol
  @hitpoints : Int32
  @power : Int32
  @tile : Tile

  def initialize(@tile, @kind, elf_power : Int32)
    @hitpoints = DEFAULT_HITPOINTS
    @power = @kind == :elf ? elf_power : DEFAULT_POWER
    @tile.unit = self
  end

  def targets(cave : Cave)
    cave.units.any? { |u| u.hitpoints > 0 && u.kind != @kind }
  end

  def enemies(cave : Cave)
    cave.units.select { |u| u.hitpoints > 0 && u.kind != @kind }.sort_by! { |u| {u.tile.y, u.tile.x} }
  end

  def enemy_neighbor(cave : Cave)
    target = nil
    [{0, -1}, {-1, 0}, {1, 0}, {0, 1}].each do |(dx, dy)|
      nx = @tile.x + dx
      ny = @tile.y + dy
      if ny >= 0 && ny < cave.map.size && nx >= 0 && nx < cave.map[0].size
        n_tile = cave.map[ny][nx]
        if n_tile && (u = n_tile.unit) && u.kind != @kind && u.hitpoints > 0
          if !target || u.hitpoints < target.hitpoints ||
             (u.hitpoints == target.hitpoints && (u.tile.y < target.tile.y || (u.tile.y == target.tile.y && u.tile.x < target.tile.x)))
            target = u
          end
        end
      end
    end
    target
  end

  def next_tile(cave : Cave)
    distances, path = find_walkable_tiles(cave.map, @tile)
    candidates = [] of Tile
    best = Int32::MAX
    enemies(cave).each do |enemy|
      [{0, -1}, {-1, 0}, {1, 0}, {0, 1}].each do |(dx, dy)|
        tx = enemy.tile.x + dx
        ty = enemy.tile.y + dy
        if ty >= 0 && ty < cave.map.size && tx >= 0 && tx < cave.map[0].size
          t = cave.map[ty][tx]
          if t && t.kind == :space && distances.has_key?(t)
            d = distances[t]
            if d < best
              best = d
              candidates = [t]
            elsif d == best
              candidates << t
            end
          end
        end
      end
    end
    return {nil, nil} if candidates.empty?
    candidates.sort_by! { |t| {t.y, t.x} }
    chosen = candidates[0]
    step = chosen
    while path[step] != @tile
      step = path[step]
    end
    {step, chosen}
  end

  def move(cave : Cave)
    return if enemy_neighbor(cave)
    step, _ = next_tile(cave)
    return unless step
    @tile.kind = :space
    @tile.unit = nil
    step.kind = @kind
    step.unit = self
    @tile = step
  end

  def attack(cave : Cave)
    if enemy = enemy_neighbor(cave)
      enemy.hitpoints -= @power
      if enemy.hitpoints <= 0
        enemy.tile.kind = :space
        enemy.tile.unit = nil
        return enemy.kind == :elf
      end
    end
    false
  end

  property :kind, :hitpoints, :tile
end

def find_walkable_tiles(game_map, start)
  distances = Hash(Tile, Int32).new
  path = Hash(Tile, Tile?).new
  queue = Deque(Tile).new
  queue << start
  distances[start] = 0
  path[start] = nil
  until queue.empty?
    current = queue.shift
    current.walkable_neighbors(game_map).each do |neighbor|
      unless distances.has_key?(neighbor)
        queue << neighbor
        distances[neighbor] = distances[current] + 1
        path[neighbor] = current
      end
    end
  end
  {distances, path}
end

class Cave
  @map : Array(Array(Tile))
  @units : Array(Unit)

  def initialize(input : Array(String), elf_power : Int32)
    @map = [] of Array(Tile)
    @units = [] of Unit
    input.each_with_index do |line, y|
      row = [] of Tile
      line.chars.each_with_index do |ch, x|
        kind = case ch
               when '.' then :space
               when '#' then :wall
               when 'E' then :elf
               when 'G' then :goblin
               else :space
               end
        tile = Tile.new(kind, x, y)
        row << tile
        @units << Unit.new(tile, kind, elf_power) if kind == :elf || kind == :goblin
      end
      @map << row
    end
  end

  def status
    hp = 0
    elves = false
    goblins = false
    @units.each do |u|
      next if u.hitpoints <= 0
      hp += u.hitpoints
      elves = true if u.kind == :elf
      goblins = true if u.kind == :goblin
    end
    {hp, elves && goblins}
  end

  def remove_dead
    @units.reject! { |u| u.hitpoints <= 0 }
  end

  def tick(stop_on_elf_death)
    remove_dead
    @units.sort_by! { |u| {u.tile.y, u.tile.x} }
    elf_died = false
    all_acted = true
    @units.each do |u|
      next if u.hitpoints <= 0
      unless u.targets(self)
        all_acted = false
        break
      end
      u.move(self)
      if u.attack(self) && stop_on_elf_death
        elf_died = true
        all_acted = false
        break
      end
    end
    {all_acted, elf_died}
  end

  property :map, :units
end

def combat(input)
  cave = Cave.new(input, 3)
  rounds = 0
  loop do
    rounds += 1
    hp, fighting = cave.status
    return (rounds - 1) * hp unless fighting
    all_acted, _ = cave.tick(false)
    rounds -= 1 unless all_acted
  end
end

lines = File.read_lines("input.txt")
puts combat(lines)
