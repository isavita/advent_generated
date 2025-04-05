
#!/bin/bash

main() {
    local filename="input.txt"
    local total_teaspoons=100
    local -a capacity durability flavor texture
    local i=0
    local c d f t

    while read -r c d f t; do
        capacity[i]=$c
        durability[i]=$d
        flavor[i]=$f
        texture[i]=$t
        ((i++))
    done < <(awk '{gsub(/,/,""); print $3, $5, $7, $9}' "$filename")

    local num_ingredients=$i
    local max_score=0
    local a b c d
    local rem_b rem_c
    local current_score total_cap total_dur total_flav total_tex

    # This assumes exactly 4 ingredients based on the loop structure
    # A more general solution for N ingredients in bash would be significantly more complex
    if (( num_ingredients != 4 )); then
         # Handle cases with different number of ingredients if necessary
         # For simplicity matching the Python structure, we proceed assuming 4
         : # No-op, or add error handling
    fi


    for a in $(seq 0 $total_teaspoons); do
        rem_b=$((total_teaspoons - a))
        for b in $(seq 0 $rem_b); do
            rem_c=$((total_teaspoons - a - b))
            for c in $(seq 0 $rem_c); do
                d=$((total_teaspoons - a - b - c))

                total_cap=$(( a * capacity[0] + b * capacity[1] + c * capacity[2] + d * capacity[3] ))
                total_dur=$(( a * durability[0] + b * durability[1] + c * durability[2] + d * durability[3] ))
                total_flav=$(( a * flavor[0] + b * flavor[1] + c * flavor[2] + d * flavor[3] ))
                total_tex=$(( a * texture[0] + b * texture[1] + c * texture[2] + d * texture[3] ))

                (( total_cap < 0 )) && total_cap=0
                (( total_dur < 0 )) && total_dur=0
                (( total_flav < 0 )) && total_flav=0
                (( total_tex < 0 )) && total_tex=0

                current_score=$(( total_cap * total_dur * total_flav * total_tex ))

                if (( current_score > max_score )); then
                    max_score=$current_score
                fi
            done
        done
    done

    echo "$max_score"
}

main
