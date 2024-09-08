def knot_hash(input)
  list = (0..255).to_a
  current_position = 0
  skip_size = 0
  lengths = input.bytes + [17, 31, 73, 47, 23]

  64.times do
    lengths.each do |length|
      list.rotate!(current_position)
      list[0...length] = list[0...length].reverse
      list.rotate!(-current_position)
      current_position = (current_position + length + skip_size) % 256
      skip_size += 1
    end
  end

  dense_hash = list.each_slice(16).map { |chunk| chunk.reduce(:^) }
  dense_hash.map { |num| num.to_s(16).rjust(2, '0') }.join
end

def count_bits(hex_string)
  hex_string.chars.sum { |char| char.to_i(16).to_s(2).count('1') }
end

key = File.read('input.txt').strip
used_squares = (0..127).sum do |i|
  count_bits(knot_hash("#{key}-#{i}"))
end

puts used_squares
