require "big"

def hex_to_bin(hex)
  bin = ""
  hex.each_char do |h|
    bin += "%04b" % h.to_i(16)
  end
  bin
end

def parse_packet(bin_str, idx)
  version = (bin_str[idx].ord - 48) << 2 | (bin_str[idx + 1].ord - 48) << 1 | (bin_str[idx + 2].ord - 48)
  type_id = (bin_str[idx + 3].ord - 48) << 2 | (bin_str[idx + 4].ord - 48) << 1 | (bin_str[idx + 5].ord - 48)
  idx += 6

  if type_id == 4
    value = 0_i64
    while bin_str[idx] == '1'
      value = value << 4 | (bin_str[idx + 1].ord - 48) << 3 | (bin_str[idx + 2].ord - 48) << 2 | (bin_str[idx + 3].ord - 48) << 1 | (bin_str[idx + 4].ord - 48)
      idx += 5
    end
    value = value << 4 | (bin_str[idx + 1].ord - 48) << 3 | (bin_str[idx + 2].ord - 48) << 2 | (bin_str[idx + 3].ord - 48) << 1 | (bin_str[idx + 4].ord - 48)
    idx += 5
    return version, idx, value
  end

  length_type_id = bin_str[idx].ord - 48
  idx += 1
  num_sub_packets = 0
  sub_packet_length = 0

  if length_type_id == 0
    15.times do
      sub_packet_length = sub_packet_length << 1 | (bin_str[idx].ord - 48)
      idx += 1
    end
  else
    11.times do
      num_sub_packets = num_sub_packets << 1 | (bin_str[idx].ord - 48)
      idx += 1
    end
  end

  values = [] of Int64
  while true
    break if length_type_id == 0 && sub_packet_length == 0
    break if length_type_id == 1 && num_sub_packets == 0
    _, new_idx, sub_value = parse_packet(bin_str, idx)
    values << sub_value

    if length_type_id == 0
      sub_packet_length -= new_idx - idx
    else
      num_sub_packets -= 1
    end
    idx = new_idx
  end

  result = 0_i64
  case type_id
  when 0
    values.each do |value|
      result += value
    end
  when 1
    result = 1_i64
    values.each do |value|
      result *= value
    end
  when 2
    result = values.first
    values.each do |value|
      result = value if value < result
    end
  when 3
    result = values.first
    values.each do |value|
      result = value if value > result
    end
  when 5
    result = values.first > values.last ? 1_i64 : 0_i64
  when 6
    result = values.first < values.last ? 1_i64 : 0_i64
  when 7
    result = values.first == values.last ? 1_i64 : 0_i64
  else
    raise "Unknown type_id"
  end

  return version, idx, result
end

File.open("input.txt") do |file|
  hex_str = file.gets_to_end.strip
  bin_str = hex_to_bin(hex_str)
  _, _, value = parse_packet(bin_str, 0)
  puts value
end