
#!/bin/bash

knot_hash() {
    local input_str=$1
    local lengths=()
    local i len char_code

    # Convert input string characters to ASCII values
    for (( i=0; i<${#input_str}; i++ )); do
        printf -v char_code "%d" "'${input_str:$i:1}"
        lengths+=("$char_code")
    done

    # Append suffix lengths
    lengths+=(17 31 73 47 23)

    # Initialize list 0-255
    local nums=()
    for i in $(seq 0 255); do
        nums[i]=$i
    done

    local pos=0
    local skip=0
    local round length j a b tmp xored block_start dense_hex hex_digit bin_str

    # 64 rounds of hashing
    for round in $(seq 1 64); do
        for length in "${lengths[@]}"; do
            # Reverse the sublist
            for (( j=0; j < length / 2; j++ )); do
                a=$(( (pos + j) % 256 ))
                b=$(( (pos + length - 1 - j) % 256 ))
                tmp=${nums[a]}
                nums[a]=${nums[b]}
                nums[b]=$tmp
            done
            pos=$(( (pos + length + skip) % 256 ))
            skip=$(( skip + 1 ))
        done
    done

    # Calculate dense hash and convert to hex
    dense_hex=""
    for i in $(seq 0 15); do
        block_start=$(( i * 16 ))
        xored=${nums[block_start]}
        for j in $(seq 1 15); do
            xored=$(( xored ^ nums[block_start + j] ))
        done
        printf -v hex_byte "%02x" "$xored"
        dense_hex+="$hex_byte"
    done

    # Convert hex hash to binary string
    bin_str=""
     for (( i=0; i<${#dense_hex}; i++ )); do
        hex_digit=${dense_hex:$i:1}
        case $hex_digit in
            0) bin_str+="0000";;
            1) bin_str+="0001";;
            2) bin_str+="0010";;
            3) bin_str+="0011";;
            4) bin_str+="0100";;
            5) bin_str+="0101";;
            6) bin_str+="0110";;
            7) bin_str+="0111";;
            8) bin_str+="1000";;
            9) bin_str+="1001";;
            a|A) bin_str+="1010";;
            b|B) bin_str+="1011";;
            c|C) bin_str+="1100";;
            d|D) bin_str+="1101";;
            e|E) bin_str+="1110";;
            f|F) bin_str+="1111";;
        esac
    done
    echo "$bin_str"

}

main() {
    local key
    read -r key < "input.txt"

    local total_ones=0
    local i hash_bin ones_in_hash

    for i in $(seq 0 127); do
        hash_bin=$(knot_hash "$key-$i")
        # Optimized counting of '1's using parameter expansion
        ones_in_hash=${hash_bin//0/} # Remove all '0's
        total_ones=$(( total_ones + ${#ones_in_hash} )) # Count remaining '1's
    done

    echo "$total_ones"
}

main
