
import sys

def hex_to_bin(hex):
    bin_str = ""
    for h in hex:
        b = int(h, 16)
        bin_str += format(b, '04b')
    return bin_str

def parse_packet(bin_str, idx):
    version = int(bin_str[idx]) << 2 | int(bin_str[idx + 1]) << 1 | int(bin_str[idx + 2])
    type_id = int(bin_str[idx + 3]) << 2 | int(bin_str[idx + 4]) << 1 | int(bin_str[idx + 5])
    idx += 6
    
    if type_id == 4:
        value = 0
        while bin_str[idx] == '1':
            value = value << 4 | int(bin_str[idx + 1]) << 3 | int(bin_str[idx + 2]) << 2 | int(bin_str[idx + 3]) << 1 | int(bin_str[idx + 4])
            idx += 5
        value = value << 4 | int(bin_str[idx + 1]) << 3 | int(bin_str[idx + 2]) << 2 | int(bin_str[idx + 3]) << 1 | int(bin_str[idx + 4])
        idx += 5
        return version, idx, value
    
    length_type_id = int(bin_str[idx])
    idx += 1
    num_sub_packets, sub_packet_length = 0, 0
    
    if length_type_id == 0:
        sub_packet_length = 0
        for i in range(15):
            sub_packet_length = sub_packet_length << 1 | int(bin_str[idx])
            idx += 1
    else:
        num_sub_packets = 0
        for i in range(11):
            num_sub_packets = num_sub_packets << 1 | int(bin_str[idx])
            idx += 1
    
    values = []
    while True:
        if length_type_id == 0 and sub_packet_length == 0:
            break
        if length_type_id == 1 and num_sub_packets == 0:
            break
        _, new_index, sub_value = parse_packet(bin_str, idx)
        values.append(sub_value)
        
        if length_type_id == 0:
            sub_packet_length -= new_index - idx
        else:
            num_sub_packets -= 1
        idx = new_index

    result = 0
    if type_id == 0:
        result = sum(values)
    elif type_id == 1:
        result = 1
        for value in values:
            result *= value
    elif type_id == 2:
        result = values[0]
        for value in values:
            if value < result:
                result = value
    elif type_id == 3:
        result = values[0]
        for value in values:
            if value > result:
                result = value
    elif type_id == 5:
        result = 0
        if values[0] > values[1]:
            result = 1
    elif type_id == 6:
        result = 0
        if values[0] < values[1]:
            result = 1
    elif type_id == 7:
        result = 0
        if values[0] == values[1]:
            result = 1
    else:
        raise Exception("Unknown typeID")
    
    return version, idx, result

if __name__ == "__main__":
    with open("input.txt", "r") as file:
        hex_str = file.read().strip()
    
    bin_str = hex_to_bin(hex_str)
    _, _, value = parse_packet(bin_str, 0)
    print(value)
