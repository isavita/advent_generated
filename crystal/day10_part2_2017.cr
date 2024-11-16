
file = File.read("input.txt")
input = file.chomp.chars.map { |c| c.ord }
lengths = input + [17, 31, 73, 47, 23]

list = (0..255).to_a
current_position = 0
skip_size = 0

64.times do
  lengths.each do |length|
    (0...length / 2).each do |i|
      start_index = (current_position + i) % 256
      end_index = (current_position + length - 1 - i) % 256
      list[start_index], list[end_index] = list[end_index], list[start_index]
    end
    current_position = (current_position + length + skip_size) % 256
    skip_size += 1
  end
end

dense_hash = (0...16).map do |i|
  list[i * 16...(i + 1) * 16].reduce(0) { |xor, num| xor ^ num }
end

hex_hash = dense_hash.map { |byte| "%02x" % byte }.join

puts hex_hash
