
def hex_to_bin(hex)
  bin = ""
  hex.each_char do |h|
    b = h.to_i(16)
    bin += "%04b" % b
  end
  bin
end

def parse_packet(bin_str, idx)
  version = (bin_str[idx].to_i * 4) | (bin_str[idx + 1].to_i * 2) | bin_str[idx + 2].to_i
  type_id = (bin_str[idx + 3].to_i * 4) | (bin_str[idx + 4].to_i * 2) | bin_str[idx + 5].to_i
  idx += 6

  if type_id == 4
    value = 0
    while bin_str[idx] == '1'
      value = (value << 4) | (bin_str[idx + 1].to_i << 3) | (bin_str[idx + 2].to_i << 2) | (bin_str[idx + 3].to_i << 1) | bin_str[idx + 4].to_i
      idx += 5
    end
    value = (value << 4) | (bin_str[idx + 1].to_i << 3) | (bin_str[idx + 2].to_i << 2) | (bin_str[idx + 3].to_i << 1) | bin_str[idx + 4].to_i
    idx += 5
    return version, idx, value
  end

  length_type_id = bin_str[idx].to_i
  idx += 1
  num_sub_packets = 0
  sub_packet_length = 0

  if length_type_id == 0
    sub_packet_length = 0
    15.times do
      sub_packet_length = (sub_packet_length << 1) | bin_str[idx].to_i
      idx += 1
    end
  else
    11.times do
      num_sub_packets = (num_sub_packets << 1) | bin_str[idx].to_i
      idx += 1
    end
  end

  values = []
  loop do
    break if (length_type_id == 0 && sub_packet_length == 0) || (length_type_id == 1 && num_sub_packets == 0)
    _, new_index, sub_value = parse_packet(bin_str, idx)
    values << sub_value

    if length_type_id == 0
      sub_packet_length -= new_index - idx
    else
      num_sub_packets -= 1
    end
    idx = new_index
  end

  result = 0
  case type_id
  when 0
    values.each { |value| result += value }
  when 1
    result = 1
    values.each { |value| result *= value }
  when 2
    result = values[0]
    values.each { |value| result = value if value < result }
  when 3
    result = values[0]
    values.each { |value| result = value if value > result }
  when 5
    result = values[0] > values[1] ? 1 : 0
  when 6
    result = values[0] < values[1] ? 1 : 0
  when 7
    result = values[0] == values[1] ? 1 : 0
  else
    raise "Unknown typeID"
  end

  return version, idx, result
end

data = File.read("input.txt").chomp
bin_str = hex_to_bin(data)
_, _, value = parse_packet(bin_str, 0)
puts value
