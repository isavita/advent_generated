
input = File.readlines('input.txt', chomp: true)

def apply_mask(value, mask)
  result = value
  mask.chars.each_with_index do |bit, index|
    if bit == '1'
      result |= (1 << (35 - index))
    elsif bit == '0'
      result &= ~(1 << (35 - index))
    end
  end
  result
end

memory = {}
mask = ''
input.each do |line|
  if line.start_with?('mask')
    mask = line.split(' = ')[1]
  else
    address, value = line.match(/mem\[(\d+)\] = (\d+)/).captures.map(&:to_i)
    memory[address] = apply_mask(value, mask)
  end
end

puts memory.values.sum
