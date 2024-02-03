
def knot_hash(input)
  list = (0..255).to_a
  lengths = input.bytes + [17, 31, 73, 47, 23]
  pos = 0
  skip = 0

  64.times do
    lengths.each do |length|
      list = list.rotate(pos)
      list[0...length] = list[0...length].reverse
      list = list.rotate(-pos)
      pos = (pos + length + skip) % list.size
      skip += 1
    end
  end

  dense_hash = list.each_slice(16).map { |block| block.reduce(:^) }
  dense_hash.map { |num| num.to_s(16).rjust(2, '0') }.join
end

input = File.read('input.txt').strip
puts knot_hash(input)
