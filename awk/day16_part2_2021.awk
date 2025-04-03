
#!/usr/bin/awk -f

# Global variables
# bin_str: Stores the binary representation of the input hex string
# hex_map: Lookup table for hex to binary conversion

# Function to convert a binary string segment to its decimal value
function bin_to_dec(bin_sub,    len, i, dec_val) {
    dec_val = 0
    len = length(bin_sub)
    for (i = 1; i <= len; i++) {
        dec_val = dec_val * 2 + substr(bin_sub, i, 1)
    }
    return dec_val
}

# Function to parse a packet starting at index idx
# result[1]: returns the new index after parsing
# result[2]: returns the calculated value of the packet
function parse_packet(idx, result,    version, type_id, literal_value, \
                      len_type_id, sub_packet_len, num_sub_packets, \
                      sub_result, values, value_count, target_idx, i, \
                      op_result, val1, val2) {

    delete values # Clear values for this packet
    value_count = 0

    # Read version and type_id
    # version = bin_to_dec(substr(bin_str, idx, 3)) # Version not needed for final calculation
    type_id = bin_to_dec(substr(bin_str, idx + 3, 3))
    idx += 6

    if (type_id == 4) { # Literal value packet
        literal_value = 0
        while (substr(bin_str, idx, 1) == "1") {
            literal_value = literal_value * 16 + bin_to_dec(substr(bin_str, idx + 1, 4))
            idx += 5
        }
        literal_value = literal_value * 16 + bin_to_dec(substr(bin_str, idx + 1, 4))
        idx += 5
        result[1] = idx
        result[2] = literal_value
        return
    } else { # Operator packet
        len_type_id = bin_to_dec(substr(bin_str, idx, 1))
        idx += 1

        if (len_type_id == 0) {
            sub_packet_len = bin_to_dec(substr(bin_str, idx, 15))
            idx += 15
            target_idx = idx + sub_packet_len
            while (idx < target_idx) {
                 parse_packet(idx, sub_result)
                 idx = sub_result[1]
                 values[++value_count] = sub_result[2]
            }
        } else {
            num_sub_packets = bin_to_dec(substr(bin_str, idx, 11))
            idx += 11
            for (i = 1; i <= num_sub_packets; i++) {
                 parse_packet(idx, sub_result)
                 idx = sub_result[1]
                 values[++value_count] = sub_result[2]
            }
        }

        # Calculate result based on type_id
        if (type_id == 0) { # Sum
            op_result = 0
            for (i = 1; i <= value_count; i++) op_result += values[i]
        } else if (type_id == 1) { # Product
            op_result = 1
            for (i = 1; i <= value_count; i++) op_result *= values[i]
        } else if (type_id == 2) { # Minimum
            op_result = values[1]
            for (i = 2; i <= value_count; i++) if (values[i] < op_result) op_result = values[i]
        } else if (type_id == 3) { # Maximum
            op_result = values[1]
            for (i = 2; i <= value_count; i++) if (values[i] > op_result) op_result = values[i]
        } else if (type_id == 5) { # Greater than
            op_result = (values[1] > values[2]) ? 1 : 0
        } else if (type_id == 6) { # Less than
            op_result = (values[1] < values[2]) ? 1 : 0
        } else if (type_id == 7) { # Equal to
            op_result = (values[1] == values[2]) ? 1 : 0
        }

        result[1] = idx
        result[2] = op_result
        return
    }
}

BEGIN {
    # Initialize hex to binary map
    hex_map["0"] = "0000"; hex_map["1"] = "0001"; hex_map["2"] = "0010"; hex_map["3"] = "0011"
    hex_map["4"] = "0100"; hex_map["5"] = "0101"; hex_map["6"] = "0110"; hex_map["7"] = "0111"
    hex_map["8"] = "1000"; hex_map["9"] = "1001"; hex_map["A"] = "1010"; hex_map["B"] = "1011"
    hex_map["C"] = "1100"; hex_map["D"] = "1101"; hex_map["E"] = "1110"; hex_map["F"] = "1111"

    # Read the entire input file content
    if ((getline hex_str < "input.txt") <= 0) {
         print "Error reading input.txt" > "/dev/stderr"
         exit 1
    }
    close("input.txt")

    # Convert hex string to binary string
    bin_str = ""
    hex_len = length(hex_str)
    for (i = 1; i <= hex_len; i++) {
        hex_char = substr(hex_str, i, 1)
        bin_str = bin_str hex_map[hex_char]
    }

    # Parse the packet starting from index 1 (AWK is 1-based)
    parse_packet(1, final_result)

    # Print the final calculated value
    print final_result[2]

    exit # Ensure script exits after BEGIN block
}

