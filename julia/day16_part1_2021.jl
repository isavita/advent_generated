
function parse_packet(bin_str, idx)
    version = parse(Int, bin_str[idx:idx+2], base=2)
    type_id = parse(Int, bin_str[idx+3:idx+5], base=2)
    idx += 6
    if type_id == 4
        while bin_str[idx] == '1'
            idx += 5
        end
        idx += 5
        return version, idx
    end
    length_type_id = parse(Int, bin_str[idx:idx])
    idx += 1
    version_sum = version
    if length_type_id == 0
        sub_packet_length = parse(Int, bin_str[idx:idx+14], base=2)
        idx += 15
        target_idx = idx + sub_packet_length
        while idx < target_idx
            sub_version, idx = parse_packet(bin_str, idx)
            version_sum += sub_version
        end
    else
        num_sub_packets = parse(Int, bin_str[idx:idx+10], base=2)
        idx += 11
        for _ = 1:num_sub_packets
            sub_version, idx = parse_packet(bin_str, idx)
            version_sum += sub_version
        end
    end
    return version_sum, idx
end

hex_str = strip(read("input.txt", String))
bin_str = join([string(parse(Int, string(h), base=16), base=2, pad=4) for h in hex_str])
version_sum, _ = parse_packet(bin_str, 1)
println(version_sum)
