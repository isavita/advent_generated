
def apply_mask_v1(mask, value)
  mask.chars.reverse.each_with_index do |char, index|
    case char
    when '0'
      value &= ~(1 << index)
    when '1'
      value |= (1 << index)
    end
  end
  value
end

def generate_addresses(mask, address)
  floating_bits = []
  mask.chars.reverse.each_with_index do |char, index|
    case char
    when '1'
      address |= (1 << index)
    when 'X'
      floating_bits << index
    end
  end

  addresses = [address]
  floating_bits.each do |bit|
    addresses += addresses.map { |addr| addr ^ (1 << bit) }
  end
  addresses
end

memory_v1 = {}
memory_v2 = {}
mask = ""

File.foreach("input.txt") do |line|
  if line.start_with?("mask")
    mask = line.split(" = ").last.chomp
  else
    address, value = line.match(/mem\[(\d+)\] = (\d+)/).captures.map(&:to_i)
    memory_v1[address] = apply_mask_v1(mask, value)
    generate_addresses(mask, address).each { |addr| memory_v2[addr] = value }
  end
end

puts memory_v1.values.sum
puts memory_v2.values.sum
