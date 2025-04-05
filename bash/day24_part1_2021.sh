
#!/bin/bash

main() {
    local -a k l m stack
    local -A constraints
    local i=0 stack_idx=0 line mod v

    while IFS= read -r line || [[ -n "$line" ]]; do
        mod=$(( i % 18 ))
        v="${line##* }" # Efficiently get last word

        if [[ "$mod" -eq 4 ]]; then
            l+=("$v")
        elif [[ "$mod" -eq 5 ]]; then
            k+=("$v")
        elif [[ "$mod" -eq 15 ]]; then
            m+=("$v")
        fi
        ((i++))
    done < "input.txt"

    local num_entries=${#l[@]}
    for (( i=0; i<num_entries; i++ )); do
        if [[ "${l[i]}" -eq 1 ]]; then
            stack[stack_idx]="$i"
            ((stack_idx++))
        elif [[ "${l[i]}" -eq 26 ]]; then
            ((stack_idx--))
            local pop_idx=${stack[stack_idx]}
            # unset 'stack[stack_idx]' # Optional: keep stack array clean

            local delta=$(( m[pop_idx] + k[i] ))
            constraints[$pop_idx]="$i $delta" # Store "j delta" as string
        fi
    done

    local -a max_vals
    # Initialize array explicitly although bash arrays are sparse
    for (( i=0; i<num_entries; i++ )); do max_vals[i]=0; done

    local j delta vmax
    for i in "${!constraints[@]}"; do # Iterate over keys of associative array
        # Read the stored "j delta" string into variables
        read -r j delta <<< "${constraints[$i]}"

        vmax=9
        # Ensure constraint (vmax + delta) is <= 9
        while (( vmax + delta > 9 )); do
            ((vmax--))
        done

        max_vals[i]=$vmax
        max_vals[j]=$(( vmax + delta ))
    done

    # Print array elements concatenated without spaces
    printf '%s' "${max_vals[@]}"
    printf '\n'
}

main
