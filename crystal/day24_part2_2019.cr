
SIDE = 5
SQUARE = SIDE * SIDE

def parse() : Array(Bool)
  res = Array(Bool).new(SQUARE, false)
  File.open("input.txt") do |file|
    row = 0
    file.each_line do |line|
      line.chomp.each_char_with_index do |char, col|
        res[row * SIDE + col] = (char == '#')
      end
      row += 1
    end
  end
  res
end

alias Space = Hash(Int32, Array(Bool))

def min_max_level(space : Space)
  min = Int32::MAX
  max = Int32::MIN
  space.each_key do |level|
    min = level if level < min
    max = level if level > max
  end
  {min, max}
end

def infested(space : Space, level : Int32, cell : Int32)
  return false unless space.has_key?(level)
  space[level][cell]
end

def clean(space : Space)
  min, max = min_max_level(space)
  count_min = 0
  count_max = 0
  (0...SQUARE).each do |cell|
    count_min += 1 if space[min][cell]
    count_max += 1 if space[max][cell]
  end
  space.delete(min) if count_min == 0
  space.delete(max) if count_max == 0
end

def next2(space : Space)
  new_space = Space.new
  min_level, max_level = min_max_level(space)
  (min_level - 1..max_level + 1).each do |level|
    new_space[level] = Array(Bool).new(SQUARE, false)
    (0...SQUARE).each do |cell|
      next if cell == 12
      row, col = cell // SIDE, cell % SIDE
      neighbours = 0
      neighbours += 1 if row == 0 && infested(space, level - 1, 7)
      neighbours += 1 if col == 0 && infested(space, level - 1, 11)
      neighbours += 1 if col == 4 && infested(space, level - 1, 13)
      neighbours += 1 if row == 4 && infested(space, level - 1, 17)
      if cell == 7
        (0...SIDE).each { |i| neighbours += 1 if infested(space, level + 1, i) }
      end
      if cell == 11
        (0...SIDE).each { |i| neighbours += 1 if infested(space, level + 1, 5 * i) }
      end
      if cell == 13
        (0...SIDE).each { |i| neighbours += 1 if infested(space, level + 1, 5 * i + SIDE - 1) }
      end
      if cell == 17
        (0...SIDE).each { |i| neighbours += 1 if infested(space, level + 1, (SIDE - 1) * SIDE + i) }
      end
      neighbours += 1 if row > 0 && cell != 17 && infested(space, level, cell - SIDE)
      neighbours += 1 if col > 0 && cell != 13 && infested(space, level, cell - 1)
      neighbours += 1 if col < SIDE - 1 && cell != 11 && infested(space, level, cell + 1)
      neighbours += 1 if row < SIDE - 1 && cell != 7 && infested(space, level, cell + SIDE)
      if infested(space, level, cell)
        new_space[level][cell] = neighbours == 1
      else
        new_space[level][cell] = (neighbours == 1 || neighbours == 2)
      end
    end
  end
  clean(new_space)
  new_space
end

input = parse()
space = Space{0 => input}
200.times { space = next2(space) }
count = 0
space.each_value do |grid|
  grid.each { |cell| count += 1 if cell }
end
puts count
