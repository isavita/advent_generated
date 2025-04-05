
#!/bin/bash

declare -A memory
mask=""

# Function to convert decimal to 36-bit binary string
dec2bin() {
    local dec=$1
    local bin=""
    # Handle 0 case explicitly
    if (( dec == 0 )); then
        printf "%036d\n" 0
        return
    fi
    # Loop for conversion
    while (( dec > 0 )); do
        bin="$((dec % 2))$bin"
        dec=$((dec / 2))
    done
    # Pad with leading zeros
    printf "%036s\n" "$bin"
}

# Recursive function to generate addresses based on floating bits
generate_addresses() {
    local template=$1
    # Find first 'X'
    if [[ "$template" != *X* ]]; then
        # Base case: no more 'X's, convert binary string to decimal and store
        local addr=$((2#$template))
        memory["$addr"]=$current_value # Use global current_value
        return
    fi

    # Find the index of the first 'X'
    local index=-1
    for (( i=0; i<${#template}; i++ )); do
        if [[ "${template:i:1}" == "X" ]]; then
            index=$i
            break
        fi
    done

    # Recurse for '0'
    generate_addresses "${template:0:index}0${template:index+1}"

    # Recurse for '1'
    generate_addresses "${template:0:index}1${template:index+1}"
}

apply_mask_v2() {
    local address=$1
    local current_mask=$2
    local addr_bin
    addr_bin=$(dec2bin "$address")

    local masked_addr_template=""
    for (( i=0; i<36; i++ )); do
        local mask_bit="${current_mask:i:1}"
        local addr_bit="${addr_bin:i:1}"

        if [[ "$mask_bit" == "1" ]]; then
            masked_addr_template+="1"
        elif [[ "$mask_bit" == "0" ]]; then
            masked_addr_template+="$addr_bit"
        else # 'X'
            masked_addr_template+="X"
        fi
    done

    # Call the recursive generator (which directly modifies the global memory array)
    generate_addresses "$masked_addr_template"
}

main() {
    local line
    local address
    local value

    while IFS= read -r line || [[ -n "$line" ]]; do
        if [[ "$line" == mask* ]]; then
            mask="${line#* = }"
        elif [[ "$line" =~ mem\[([0-9]+)\]\ =\ ([0-9]+) ]]; then
            address=${BASH_REMATCH[1]}
            value=${BASH_REMATCH[2]}
            current_value=$value # Set global value for generate_addresses
            apply_mask_v2 "$address" "$mask"
        fi
    done < "input.txt"

    local total=0
    for val in "${memory[@]}"; do
        total=$((total + val))
    done
    echo "$total"
}

main
