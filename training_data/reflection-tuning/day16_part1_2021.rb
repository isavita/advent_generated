def hex_to_binary(hex)
  hex.chars.map { |c| c.to_i(16).to_s(2).rjust(4, '0') }.join
end

def parse_packet(binary, pos = 0)
  version = binary[pos, 3].to_i(2)
  type_id = binary[pos + 3, 3].to_i(2)
  pos += 6
  version_sum = version

  if type_id == 4 # Literal value
    value = ''
    loop do
      group = binary[pos, 5]
      value += group[1, 4]
      pos += 5
      break if group[0] == '0'
    end
  else # Operator
    length_type_id = binary[pos].to_i
    pos += 1
    if length_type_id == 0
      length = binary[pos, 15].to_i(2)
      pos += 15
      end_pos = pos + length
      while pos < end_pos
        sub_version_sum, pos = parse_packet(binary, pos)
        version_sum += sub_version_sum
      end
    else
      num_subpackets = binary[pos, 11].to_i(2)
      pos += 11
      num_subpackets.times do
        sub_version_sum, pos = parse_packet(binary, pos)
        version_sum += sub_version_sum
      end
    end
  end

  [version_sum, pos]
end

input = File.read('input.txt').strip
binary = hex_to_binary(input)
version_sum, _ = parse_packet(binary)
puts version_sum
