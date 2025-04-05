
#!/bin/bash

shopt -s extglob # Enable extended globbing for regex-like patterns if needed, though not strictly used here

# Function to check for ABBA pattern (xyyx where x != y)
has_abba() {
    local s=$1
    local len=${#s}
    # Iterate through all 4-char substrings
    for (( i=0; i<=len-4; i++ )); do
        local sub=${s:i:4}
        local c1=${sub:0:1}
        local c2=${sub:1:1}
        # Check pattern: c1 == c4, c2 == c3, c1 != c2
        if [[ "${sub:3:1}" == "$c1" && "${sub:2:1}" == "$c2" && "$c1" != "$c2" ]]; then
            return 0 # Found ABBA (success/true)
        fi
    done
    return 1 # No ABBA found (failure/false)
}

main() {
    local count=0
    while IFS= read -r line || [[ -n "$line" ]]; do
        local abba_outside=false
        local abba_inside=false
        local hypernet=false

        # Split the line into parts based on brackets using parameter expansion
        local line_replaced_brackets="${line//\[/ ]}" # Replace [ with space
        line_replaced_brackets="${line_replaced_brackets//\]/ ]}" # Replace ] with space followed by ]
        
        local parts=()
        local current_part=""
        local in_bracket=false
        
        # Manually parse to correctly identify bracketed vs non-bracketed parts
        for (( i=0; i<${#line}; i++ )); do
            local char="${line:i:1}"
            if [[ "$char" == "[" ]]; then
                parts+=("$current_part") # Add part before bracket
                current_part=""
                in_bracket=true
            elif [[ "$char" == "]" ]]; then
                parts+=("[$current_part]") # Add part inside bracket (marked)
                current_part=""
                in_bracket=false
            else
                current_part+="$char"
            fi
        done
        parts+=("$current_part") # Add the last part

        # Process parts
        for part in "${parts[@]}"; do
            if [[ "$part" == \[* ]]; then 
                # Inside hypernet - remove brackets for checking
                local inner_part="${part:1:${#part}-2}" 
                if has_abba "$inner_part"; then
                    abba_inside=true
                    break # If ABBA found inside, this IP fails TLS
                fi
            else 
                # Outside hypernet
                if ! $abba_outside; then # Only need to find one outside
                   if has_abba "$part"; then
                       abba_outside=true
                   fi
                fi
            fi
        done

        if $abba_outside && ! $abba_inside; then
            ((count++))
        fi
    done < "input.txt"

    echo "$count"
}

# Entry point
main

