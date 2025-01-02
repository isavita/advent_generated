
function hex2bin(hex)
    bin = ""
    for h in hex
        b = parse(UInt8, h, base=16)
        bin *= lpad(string(b, base=2), 4, '0')
    end
    return bin
end

function parse_packet(bin_str, idx)
    version = parse(Int, bin_str[idx:idx+2], base=2)
    type_id = parse(Int, bin_str[idx+3:idx+5], base=2)
    idx += 6

    if type_id == 4
        value = 0
        while bin_str[idx] == '1'
            value = value << 4 + parse(Int, bin_str[idx+1:idx+4], base=2)
            idx += 5
        end
        value = value << 4 + parse(Int, bin_str[idx+1:idx+4], base=2)
        idx += 5
        return version, idx, value
    end

    length_type_id = parse(Int, bin_str[idx])
    idx += 1
    
    if length_type_id == 0
        sub_packet_length = parse(Int, bin_str[idx:idx+14], base=2)
        idx += 15
    else
        num_sub_packets = parse(Int, bin_str[idx:idx+10], base=2)
        idx += 11
    end

    values = Int64[]
    if length_type_id == 0
        target_idx = idx + sub_packet_length
        while idx < target_idx
            _, new_idx, sub_value = parse_packet(bin_str, idx)
            push!(values, sub_value)
            idx = new_idx
        end
    else
        for _ = 1:num_sub_packets
            _, new_idx, sub_value = parse_packet(bin_str, idx)
            push!(values, sub_value)
            idx = new_idx
        end
    end

    if type_id == 0
        result = sum(values)
    elseif type_id == 1
        result = prod(values)
    elseif type_id == 2
        result = minimum(values)
    elseif type_id == 3
        result = maximum(values)
    elseif type_id == 5
        result = values[1] > values[2] ? 1 : 0
    elseif type_id == 6
        result = values[1] < values[2] ? 1 : 0
    elseif type_id == 7
        result = values[1] == values[2] ? 1 : 0
    else
        error("Unknown typeID")
    end

    return version, idx, result
end

data = read("input.txt", String)
hex_str = strip(data)
bin_str = hex2bin(hex_str)
_, _, value = parse_packet(bin_str, 1)
println(value)
