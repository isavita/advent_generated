
#!/bin/bash

main() {
    local total_diff_code_mem=0
    local total_diff_encoded_code=0
    local line

    while IFS= read -r line || [[ -n "$line" ]]; do
        local line_len=${#line}
        local i=1
        local line_diff_code_mem=2 # Start with 2 for the surrounding quotes ""

        # Calculate code - memory difference
        while (( i < line_len - 1 )); do
            if [[ "${line:i:1}" == "\\" ]]; then
                local next_char="${line:i+1:1}"
                if [[ "$next_char" == "\\" || "$next_char" == "\"" ]]; then
                    (( line_diff_code_mem += 1 ))
                    (( i += 2 ))
                elif [[ "$next_char" == "x" ]]; then
                    (( line_diff_code_mem += 3 ))
                    (( i += 4 ))
                else
                    # Assuming valid input per Python eval rules
                    (( i += 1 ))
                fi
            else
                (( i += 1 ))
            fi
        done
        (( total_diff_code_mem += line_diff_code_mem ))

        # Calculate encoded - code difference
        local temp_no_slash=${line//\\}
        local num_slash=$(( line_len - ${#temp_no_slash} ))
        local temp_no_quote=${line//\"}
        local num_quote=$(( line_len - ${#temp_no_quote} ))
        local line_diff_encoded_code=$(( num_slash + num_quote + 2 ))
        (( total_diff_encoded_code += line_diff_encoded_code ))

    done < "input.txt"

    echo "$total_diff_code_mem"
    echo "$total_diff_encoded_code"
}

main
