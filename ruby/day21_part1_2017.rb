
def flip(pattern)
  pattern.map(&:reverse)
end

def rotate(pattern)
  pattern.transpose.map(&:reverse)
end

def transform(pattern)
  [pattern, flip(pattern)].flat_map { |p| [p, rotate(p), rotate(rotate(p)), rotate(rotate(rotate(p)))] }.uniq
end

def pattern_to_key(pattern)
  pattern.map { |row| row.join }.join("/")
end

def key_to_pattern(key)
  key.split("/").map(&:chars)
end

def enhance(grid, rules)
  size = grid.length
  new_size = size % 2 == 0 ? size / 2 * 3 : size / 3 * 4
  new_grid = Array.new(new_size) { Array.new(new_size) }
  block_size = size % 2 == 0 ? 2 : 3
  (0...size / block_size).each do |i|
    (0...size / block_size).each do |j|
      block = grid[i*block_size, block_size].map { |row| row[j*block_size, block_size] }
      key = pattern_to_key(block)
      transformed_block = key_to_pattern(rules[key])
      transformed_block.each_with_index do |row, r|
        row.each_with_index do |val, c|
          new_grid[i*block_size*transformed_block.length/block_size + r][j*block_size*transformed_block.length/block_size + c] = val
        end
      end
    end
  end
  new_grid
end

def count_on(grid)
  grid.sum { |row| row.count('#') }
end

initial_pattern = [".#.", "..#", "###"]
grid = initial_pattern.map(&:chars)

rules_input = File.read("input.txt").split("\n")
rules = {}
rules_input.each do |rule|
  key, value = rule.split(" => ")
  transform(key_to_pattern(key)).each { |pattern| rules[pattern_to_key(pattern)] = value }
end

5.times { grid = enhance(grid, rules) }

puts count_on(grid)
