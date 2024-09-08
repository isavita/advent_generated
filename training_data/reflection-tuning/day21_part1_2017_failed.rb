def parse_rules(input)
  input.each_with_object({}) do |line, rules|
    pattern, result = line.split(' => ')
    variations = input_variations(pattern)
    variations.each { |v| rules[v] = result.split('/') }
  end
end

def input_variations(pattern)
  rows = pattern.split('/')
  variations = []
  
  # Original
  variations << rows
  
  # Rotations
  3.times do
    rows = rows.map(&:chars).transpose.map(&:reverse).map(&:join)
    variations << rows
  end
  
  # Flips
  [rows, rows.map(&:reverse)].each do |flipped|
    variations << flipped
    3.times do
      flipped = flipped.map(&:chars).transpose.map(&:reverse).map(&:join)
      variations << flipped
    end
  end
  
  variations.map { |v| v.join('/') }.uniq
end

def enhance(grid, rules)
  size = grid.size
  chunk_size = size.even? ? 2 : 3
  new_size = size / chunk_size * (chunk_size + 1)
  new_grid = Array.new(new_size) { Array.new(new_size, '.') }

  (0...size).step(chunk_size) do |i|
    (0...size).step(chunk_size) do |j|
      chunk = grid[i...i+chunk_size].map { |row| row[j...j+chunk_size].join }
      pattern = chunk.join('/')
      new_chunk = rules[pattern]
      
      new_i = i / chunk_size * (chunk_size + 1)
      new_j = j / chunk_size * (chunk_size + 1)
      new_chunk.each_with_index do |row, di|
        row.chars.each_with_index do |char, dj|
          new_grid[new_i + di][new_j + dj] = char
        end
      end
    end
  end

  new_grid
end

def count_on_pixels(grid)
  grid.sum { |row| row.count('#') }
end

input = File.readlines('input.txt').map(&:strip)
rules = parse_rules(input)
grid = ['.#.', '..#', '###']

5.times do
  grid = enhance(grid, rules)
end

puts count_on_pixels(grid)
