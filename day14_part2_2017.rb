
require 'set'

def knot_hash(input)
  list = (0..255).to_a
  lengths = input.chars.map(&:ord) + [17, 31, 73, 47, 23]
  current_pos = 0
  skip_size = 0

  64.times do
    lengths.each do |length|
      list.rotate!(current_pos)
      list[0, length] = list[0, length].reverse
      list.rotate!(-current_pos)

      current_pos = (current_pos + length + skip_size) % list.length
      skip_size += 1
    end
  end

  dense_hash = list.each_slice(16).map { |block| block.reduce(:^) }
  dense_hash.map { |num| num.to_s(16).rjust(2, '0') }.join
end

def hex_to_bin(hex)
  hex.hex.to_s(2).rjust(4, '0')
end

def used_squares(key)
  (0..127).map do |i|
    hash = knot_hash("#{key}-#{i}")
    hash.chars.map { |char| hex_to_bin(char) }.join
  end.join.count('1')
end

def find_region(grid, x, y)
  return if x < 0 || y < 0 || x >= grid.length || y >= grid[x].length || grid[x][y] != '1'

  grid[x][y] = '0'

  find_region(grid, x + 1, y)
  find_region(grid, x - 1, y)
  find_region(grid, x, y + 1)
  find_region(grid, x, y - 1)
end

def count_regions(key)
  grid = (0..127).map do |i|
    hash = knot_hash("#{key}-#{i}")
    hash.chars.map { |char| hex_to_bin(char) }.join
  end.map(&:chars)

  regions = 0

  grid.each_with_index do |row, x|
    row.each_with_index do |cell, y|
      if cell == '1'
        find_region(grid, x, y)
        regions += 1
      end
    end
  end

  regions
end

key = File.read('input.txt').strip
puts used_squares(key)
puts count_regions(key)
