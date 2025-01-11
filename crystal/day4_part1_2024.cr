
record Direction, dx : Int32, dy : Int32

def get_all_directions
  [
    Direction.new(0, 1),
    Direction.new(1, 0),
    Direction.new(1, 1),
    Direction.new(-1, 1),
    Direction.new(0, -1),
    Direction.new(-1, 0),
    Direction.new(-1, -1),
    Direction.new(1, -1),
  ]
end

def check_word(grid, word, x, y, d)
  return false if x < 0 || y < 0 || x >= grid.size || y >= grid[0].size

  word.size.times do |i|
    new_x, new_y = x + (d.dx * i), y + (d.dy * i)
    return false if new_x < 0 || new_y < 0 || new_x >= grid.size || new_y >= grid[0].size
    return false if grid[new_x][new_y] != word[i]
  end
  true
end

def count_occurrences(grid, word)
  count = 0
  directions = get_all_directions

  grid.size.times do |i|
    grid[i].size.times do |j|
      directions.each do |dir|
        count += 1 if check_word(grid, word, i, j, dir)
      end
    end
  end
  count
end

file = File.open("input.txt")
grid = [] of String
file.each_line do |line|
  line = line.strip
  grid << line unless line.empty?
end

count = count_occurrences(grid, "XMAS")
puts "XMAS appears #{count} times in the word search"
