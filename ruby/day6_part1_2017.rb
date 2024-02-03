blocks = File.read("input.txt").split.map(&:to_i)

seen = []
cycles = 0

until seen.include?(blocks)
  seen << blocks.dup
  max_blocks = blocks.max
  max_index = blocks.index(max_blocks)
  blocks[max_index] = 0

  max_blocks.times do
    max_index = (max_index + 1) % blocks.length
    blocks[max_index] += 1
  end

  cycles += 1
end

puts cycles