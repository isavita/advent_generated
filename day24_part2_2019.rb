
Side = 5
Square = Side * Side

def parse
  res = Array.new(Square, false)

  File.open("input.txt", "r") do |file|
    file.each_with_index do |line, row|
      line.chomp.chars.each_with_index do |char, col|
        res[row * Side + col] = true if char == '#'
      end
    end
  end

  res
end

def next2(space)
  new_space = {}

  min_level, max_level = space.keys.minmax

  (min_level - 1..max_level + 1).each do |level|
    new_space[level] = Array.new(Square, false)

    (0...Square).each do |cell|
      next if cell == 12

      row, col = cell.divmod(Side)
      neighbours = 0

      neighbours += 1 if row == 0 && space[level - 1]&.[](7)
      neighbours += 1 if col == 0 && space[level - 1]&.[](11)
      neighbours += 1 if col == 4 && space[level - 1]&.[](13)
      neighbours += 1 if row == 4 && space[level - 1]&.[](17)

      if cell == 7
        (0...Side).each { |i| neighbours += 1 if space[level + 1]&.[](i) }
      end

      if cell == 11
        (0...Side).each { |i| neighbours += 1 if space[level + 1]&.[](5 * i) }
      end

      if cell == 13
        (0...Side).each { |i| neighbours += 1 if space[level + 1]&.[](5 * i + Side - 1) }
      end

      if cell == 17
        (0...Side).each { |i| neighbours += 1 if space[level + 1]&.[]((Side - 1) * Side + i) }
      end

      neighbours += 1 if row > 0 && cell != 17 && space[level]&.[](cell - Side)
      neighbours += 1 if col > 0 && cell != 13 && space[level]&.[](cell - 1)
      neighbours += 1 if col < Side - 1 && cell != 11 && space[level]&.[](cell + 1)
      neighbours += 1 if row < Side - 1 && cell != 7 && space[level]&.[](cell + Side)

      if space[level]&.[](cell) && neighbours != 1
        new_space[level][cell] = false
        next
      end

      if !space[level]&.[](cell) && (neighbours == 1 || neighbours == 2)
        new_space[level][cell] = true
        next
      end

      new_space[level][cell] = space[level]&.[](cell)
    end
  end

  clean(new_space)

  new_space
end

def clean(space)
  min, max = space.keys.minmax

  count_min = space[min].count(true)
  count_max = space[max].count(true)

  space.delete(min) if count_min.zero?
  space.delete(max) if count_max.zero?
end

def infested(space, level, cell)
  space[level]&.[](cell) || false
end

input = parse
space = { 0 => input }

200.times { space = next2(space) }

count = space.values.sum { |grid| grid.count(true) }
puts count
