def knot_hash(input)
  lengths = input.chars.map(&:ord) + [17, 31, 73, 47, 23]
  list = (0..255).to_a
  current_position = 0
  skip_size = 0

  64.times do
    lengths.each do |length|
      list.rotate!(current_position)
      list[0...length] = list[0...length].reverse
      list.rotate!(-current_position)
      current_position = (current_position + length + skip_size) % 256
      skip_size += 1
    end
  end

  list.each_slice(16).map { |chunk| chunk.reduce(:^) }.map { |n| n.to_s(16).rjust(2, '0') }.join
end

def generate_grid(key)
  (0..127).map do |i|
    knot_hash("#{key}-#{i}").chars.map { |c| c.to_i(16).to_s(2).rjust(4, '0') }.join.chars.map(&:to_i)
  end
end

def count_used_squares(grid)
  grid.sum { |row| row.count(1) }
end

def count_regions(grid)
  regions = 0
  visited = Array.new(128) { Array.new(128, false) }

  def dfs(grid, visited, i, j)
    return if i < 0 || i >= 128 || j < 0 || j >= 128 || visited[i][j] || grid[i][j] == 0
    visited[i][j] = true
    dfs(grid, visited, i-1, j)
    dfs(grid, visited, i+1, j)
    dfs(grid, visited, i, j-1)
    dfs(grid, visited, i, j+1)
  end

  128.times do |i|
    128.times do |j|
      if grid[i][j] == 1 && !visited[i][j]
        regions += 1
        dfs(grid, visited, i, j)
      end
    end
  end

  regions
end

key = File.read('input.txt').strip
grid = generate_grid(key)

puts "Part 1: #{count_used_squares(grid)}"
puts "Part 2: #{count_regions(grid)}"
