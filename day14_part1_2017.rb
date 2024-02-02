
require 'digest'

def knot_hash(input)
  lengths = input.bytes + [17, 31, 73, 47, 23]
  list = (0..255).to_a
  pos = 0
  skip = 0

  64.times do
    lengths.each do |len|
      list.rotate!(pos)
      list[0, len] = list[0, len].reverse
      list.rotate!(-pos)

      pos += len + skip
      skip += 1
    end
  end

  dense_hash = list.each_slice(16).map { |block| block.reduce(:^) }
  dense_hash.map { |n| "%02x" % n }.join
end

def count_used_squares(input)
  grid = []

  128.times do |i|
    hash = knot_hash("#{input}-#{i}")
    binary = hash.hex.to_s(2).rjust(128, '0')
    grid << binary.chars
  end

  grid.flatten.count('1')
end

input = File.read('input.txt').strip
puts count_used_squares(input)
