
#!/bin/bash

hash_string() {
    local s="$1"
    local h=0
    local char_val
    local i
    for (( i=0; i<${#s}; i++ )); do
        char_val=$(printf "%d" "'${s:i:1}")
        h=$(( (h + char_val) * 17 % 256 ))
    done
    echo "$h"
}

main() {
    local input_file="input.txt"
    local line
    IFS= read -r line < "$input_file"

    local -a steps
    IFS=',' read -r -a steps <<< "$line"

    # Use associative array for boxes; keys are box numbers (0-255)
    # Values are space-separated strings like "label1=focal1 label2=focal2 ..."
    # Using associative array allows sparse storage (only boxes with lenses exist)
    declare -A boxes

    local step label op num box_idx current_box_content
    local -a lenses temp_lenses
    local lens found slot focal_length power

    for step in "${steps[@]}"; do
        if [[ "$step" == *"-"* ]]; then
            label="${step%-*}"
            op="-"
            num=""
        else
            label="${step%=*}"
            op="="
            num="${step#*=}"
        fi

        box_idx=$(hash_string "$label")
        current_box_content="${boxes[$box_idx]}"
        # Read space-separated lenses into an array
        IFS=' ' read -r -a lenses <<< "$current_box_content"
        temp_lenses=() # Reset temp array for this step
        found=0

        if [[ "$op" == "-" ]]; then
            for lens in "${lenses[@]}"; do
                if [[ "${lens%%=*}" != "$label" ]]; then
                    temp_lenses+=("$lens")
                fi
            done
        elif [[ "$op" == "=" ]]; then
            for lens in "${lenses[@]}"; do
                if [[ "${lens%%=*}" == "$label" ]]; then
                    temp_lenses+=("$label=$num") # Update lens
                    found=1
                else
                    temp_lenses+=("$lens") # Keep existing lens
                fi
            done
            if [[ "$found" -eq 0 ]]; then
                temp_lenses+=("$label=$num") # Add new lens
            fi
        fi

        # Join temp_lenses back into a space-separated string
        local joined_lenses
        printf -v joined_lenses '%s ' "${temp_lenses[@]}"
        # Store trimmed string (removes trailing space)
        boxes["$box_idx"]="${joined_lenses% }"

        # Optimization: remove box entry if it becomes empty
        if [[ -z "${boxes[$box_idx]}" ]]; then
            unset boxes["$box_idx"]
        fi

    done

    # Calculate power
    local total_power=0
    local i # Box index

    # Iterate over the keys (box indices) of the associative array
    for i in "${!boxes[@]}"; do
        current_box_content="${boxes[$i]}"
        IFS=' ' read -r -a lenses <<< "$current_box_content"
        slot=1
        for lens in "${lenses[@]}"; do
            focal_length="${lens#*=}"
            power=$(( (i + 1) * slot * focal_length ))
            total_power=$(( total_power + power ))
            slot=$(( slot + 1 ))
        done
    done

    echo "$total_power"
}

main
