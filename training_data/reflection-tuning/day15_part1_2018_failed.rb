class Unit
  attr_reader :type, :x, :y, :hp, :attack_power

  def initialize(type, x, y)
    @type = type
    @x = x
    @y = y
    @hp = 200
    @attack_power = 3
  end

  def move(new_x, new_y)
    @x = new_x
    @y = new_y
  end

  def take_damage(damage)
    @hp -= damage
  end

  def alive?
    @hp > 0
  end
end

class BattleSimulator
  def initialize(input)
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
  end

  def simulate
    rounds = 0
    loop do
      @units.sort_by! { |u| [u.y, u.x] }
      @units.each do |unit|
        return rounds * remaining_hp unless unit.alive? && enemies_exist?(unit)
        move(unit)
        attack(unit)
      end
      @units.reject! { |u| !u.alive? }
      rounds += 1
    end
  end

  private

  def enemies_exist?(unit)
    @units.any? { |u| u.type != unit.type && u.alive? }
  end

  def move(unit)
    targets = @units.select { |u| u.type != unit.type && u.alive? }
    return if targets.empty?

    in_range = targets.flat_map { |t| adjacent_positions(t.x, t.y) }
                      .select { |x, y| @map[y][x] == '.' }
                      .uniq

    return if in_range.include?([unit.x, unit.y])

    path = find_path(unit, in_range)
    unit.move(*path.first) if path && !path.empty?
  end

  def attack(unit)
    targets = @units.select { |u| u.type != unit.type && u.alive? && adjacent?(unit, u) }
    return if targets.empty?

    target = targets.min_by { |t| [t.hp, t.y, t.x] }
    target.take_damage(unit.attack_power)
  end

  def adjacent?(unit1, unit2)
    (unit1.x - unit2.x).abs + (unit1.y - unit2.y).abs == 1
  end

  def adjacent_positions(x, y)
    [[x, y-1], [x-1, y], [x+1, y], [x, y+1]]
  end

  def find_path(unit, targets)
    queue = [[unit.x, unit.y, []]]
    visited = Set.new
    
    until queue.empty?
      x, y, path = queue.shift
      return path if targets.include?([x, y])

      adjacent_positions(x, y).each do |nx, ny|
        next if visited.include?([nx, ny]) || @map[ny][nx] != '.' || @units.any? { |u| u.x == nx && u.y == ny && u.alive? }
        visited.add([nx, ny])
        queue << [nx, ny, path + [[nx, ny]]]
      end
    end

    nil
  end

  def remaining_hp
    @units.select(&:alive?).sum(&:hp)
  end
end

input = File.read('input.txt')
simulator = BattleSimulator.new(input)
puts simulator.simulate
