
Side = 5
Square = Side * Side

def parse
  res = Array(Bool).new(Square, false)

  File.open("input.txt") do |file|
    file.each_line.with_index do |line, row|
      line.chars.each_with_index do |char, col|
        res[row * Side + col] = true if char == '#'
      end
    end
  end

  res
end

def next1(grid : Array(Bool)) : Array(Bool)
  new_grid = Array(Bool).new(Square, false)

  (0...Square).each do |i|
    row, col = i.divmod(Side)
    neighbours = 0

    neighbours += 1 if row > 0 && grid[i - Side]
    neighbours += 1 if row < Side - 1 && grid[i + Side]
    neighbours += 1 if col > 0 && grid[i - 1]
    neighbours += 1 if col < Side - 1 && grid[i + 1]

    if grid[i] && neighbours != 1
      new_grid[i] = false
    elsif !grid[i] && (neighbours == 1 || neighbours == 2)
      new_grid[i] = true
    else
      new_grid[i] = grid[i]
    end
  end

  new_grid
end

def biodiversity(grid : Array(Bool)) : Int32
  bio = 0
  (0...Square).each do |i|
    bio += 1 << i if grid[i]
  end
  bio
end

appeared = Hash(Array(Bool), Nil).new

grid = parse
appeared[grid] = nil

loop do
  grid = next1(grid)
  break if appeared.has_key?(grid)

  appeared[grid] = nil
end

puts biodiversity(grid)
