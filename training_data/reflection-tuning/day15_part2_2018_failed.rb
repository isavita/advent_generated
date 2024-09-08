class Unit
  attr_reader :type, :x, :y
  attr_accessor :hp

  def initialize(type, x, y)
    @type = type
    @x = x
    @y = y
    @hp = 200
  end

  def move(dx, dy)
    @x += dx
    @y += dy
  end

  def alive?
    @hp > 0
  end
end

class Combat
  def initialize(input, elf_attack_power = 3)
    @map = input.split("\n").map(&:chars)
    @units = []
    @map.each_with_index do |row, y|
      row.each_with_index do |cell, x|
        if cell == 'E' || cell == 'G'
          @units << Unit.new(cell, x, y)
          @map[y][x] = '.'
        end
      end
    end
    @elf_attack_power = elf_attack_power
  end

  def simulate
    rounds = 0
    loop do
      @units.sort_by! { |u| [u.y, u.x] }
      @units.each do |unit|
        return rounds * remaining_hp if combat_ended?
        next unless unit.alive?
        
        targets = @units.select { |u| u.type != unit.type && u.alive? }
        return rounds * remaining_hp if targets.empty?

        move(unit, targets) unless adjacent_enemy?(unit)
        attack(unit) if adjacent_enemy?(unit)
      end
      rounds += 1
    end
  end

  def move(unit, targets)
    in_range = targets.flat_map { |t| adjacent_squares(t.x, t.y) }
    paths = bfs(unit.x, unit.y, in_range)
    return if paths.empty?

    shortest = paths.min_by { |p| [p.length, p[-1][1], p[-1][0]] }
    unit.move(shortest[1][0] - unit.x, shortest[1][1] - unit.y)
  end

  def attack(unit)
    enemies = adjacent_enemies(unit)
    target = enemies.min_by { |e| [e.hp, e.y, e.x] }
    attack_power = unit.type == 'E' ? @elf_attack_power : 3
    target.hp -= attack_power
    if target.hp <= 0
      @map[target.y][target.x] = '.'
    end
  end

  def bfs(start_x, start_y, goals)
    queue = [[start_x, start_y]]
    visited = { [start_x, start_y] => [[start_x, start_y]] }
    
    until queue.empty?
      x, y = queue.shift
      return [visited[[x, y]]] if goals.include?([x, y])

      [[0, 1], [0, -1], [1, 0], [-1, 0]].each do |dx, dy|
        nx, ny = x + dx, y + dy
        next if @map[ny][nx] != '.' || visited.key?([nx, ny])
        next if @units.any? { |u| u.x == nx && u.y == ny && u.alive? }

        visited[[nx, ny]] = visited[[x, y]] + [[nx, ny]]
        queue << [nx, ny]
      end
    end
    []
  end

  def adjacent_squares(x, y)
    [[x, y-1], [x-1, y], [x+1, y], [x, y+1]].select { |nx, ny| @map[ny][nx] == '.' }
  end

  def adjacent_enemy?(unit)
    adjacent_enemies(unit).any?
  end

  def adjacent_enemies(unit)
    @units.select do |u|
      u.alive? && u.type != unit.type &&
        (u.x - unit.x).abs + (u.y - unit.y).abs == 1
    end
  end

  def combat_ended?
    @units.select(&:alive?).map(&:type).uniq.size == 1
  end

  def remaining_hp
    @units.select(&:alive?).sum(&:hp)
  end

  def elves_all_alive?
    @units.count { |u| u.type == 'E' } == @units.count { |u| u.type == 'E' && u.alive? }
  end
end

def solve_part1(input)
  Combat.new(input).simulate
end

def solve_part2(input)
  elf_power = 4
  loop do
    combat = Combat.new(input, elf_power)
    outcome = combat.simulate
    return outcome if combat.elves_all_alive?
    elf_power += 1
  end
end

input = File.read('input.txt')
puts "Part 1: #{solve_part1(input)}"
puts "Part 2: #{solve_part2(input)}"
