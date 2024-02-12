
def hex_to_bin(hex : String) : String
  bin = ""
  hex.each_char do |h|
    b = h.to_i(16)
    bin += b.to_s(2).rjust(4, '0')
  end
  bin
end

def parse_packet(bin_str : String, idx : Int) : Tuple(Int32, Int32)
  version = (bin_str[idx].to_i - '0'.to_i) << 2 | (bin_str[idx + 1].to_i - '0'.to_i) << 1 | (bin_str[idx + 2].to_i - '0'.to_i)
  type_id = (bin_str[idx + 3].to_i - '0'.to_i) << 2 | (bin_str[idx + 4].to_i - '0'.to_i) << 1 | (bin_str[idx + 5].to_i - '0'.to_i)
  idx += 6

  if type_id == 4
    while bin_str[idx] == '1'
      idx += 5
    end
    idx += 5
    return version, idx
  end

  length_type_id = bin_str[idx].to_i
  idx += 1
  num_sub_packets = 0
  sub_packet_length = 0

  if length_type_id == 0
    sub_packet_length = 0
    15.times do
      sub_packet_length = sub_packet_length << 1 | bin_str[idx].to_i
      idx += 1
    end
  else
    11.times do
      num_sub_packets = num_sub_packets << 1 | bin_str[idx].to_i
      idx += 1
    end
  end

  version_sum = version
  loop do
    break if length_type_id == 0 && sub_packet_length == 0
    break if length_type_id == 1 && num_sub_packets == 0

    sub_version, new_index = parse_packet(bin_str, idx)
    version_sum += sub_version

    if length_type_id == 0
      sub_packet_length -= new_index - idx
    else
      num_sub_packets -= 1
    end
    idx = new_index
  end

  return version_sum, idx # Explicitly return the tuple here
end

data = File.read("input.txt").chomp
hex_str = data.strip
bin_str = hex_to_bin(hex_str)
version_sum, _ = parse_packet(bin_str, 0)
puts version_sum
