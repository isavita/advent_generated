def apply_mask_v1(value, mask)
  mask.chars.reverse.each_with_index do |bit, i|
    case bit
    when '0' then value &= ~(1 << i)
    when '1' then value |= (1 << i)
    end
  end
  value
end

def apply_mask_v2(address, mask, value, memory, index = 0)
  if index == 36
    memory[address] = value
    return
  end

  case mask[index]
  when '0'
    apply_mask_v2(address, mask, value, memory, index + 1)
  when '1'
    apply_mask_v2(address | (1 << (35 - index)), mask, value, memory, index + 1)
  when 'X'
    apply_mask_v2(address & ~(1 << (35 - index)), mask, value, memory, index + 1)
    apply_mask_v2(address | (1 << (35 - index)), mask, value, memory, index + 1)
  end
end

def solve(input)
  memory_v1 = {}
  memory_v2 = {}
  mask = ''

  input.each_line do |line|
    if line.start_with?('mask')
      mask = line.split(' = ').last.strip
    else
      address, value = line.scan(/\d+/).map(&:to_i)
      memory_v1[address] = apply_mask_v1(value, mask)
      apply_mask_v2(address, mask, value, memory_v2)
    end
  end

  [memory_v1.values.sum, memory_v2.values.sum]
end

input = File.read('input.txt')
part1, part2 = solve(input)
puts "Part 1: #{part1}"
puts "Part 2: #{part2}"
