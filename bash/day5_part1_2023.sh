
#!/bin/bash

set -euo pipefail

main() {
    local line
    declare -a seeds
    declare -a all_src_starts
    declare -a all_dest_starts
    declare -a all_lengths
    declare -a map_boundaries=(0) # Start index of each map's ranges in the flat arrays

    local current_map_index=-1
    local in_map_section=false

    while IFS= read -r line || [[ -n "$line" ]]; do
        # Trim leading/trailing whitespace (optional, for robustness)
        line=$(echo "$line" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')

        if [[ -z "$line" ]]; then
            in_map_section=false # Gap between maps
            continue
        fi

        if [[ $line == "seeds:"* ]]; then
            read -ra seeds <<< "${line#seeds: }"
        elif [[ $line == *" map:"* ]]; then
            # If starting a new map section, record the boundary
            if ! $in_map_section; then
                 ((current_map_index++))
                 map_boundaries+=("${#all_src_starts[@]}")
                 in_map_section=true
            fi
         elif $in_map_section; then
            # Must be range data within the current map section
            read -r dest src len <<< "$line"
            # Basic check if line contained 3 numbers
            if [[ -n "$dest" && -n "$src" && -n "$len" ]]; then
                 all_dest_starts+=("$dest")
                 all_src_starts+=("$src")
                 all_lengths+=("$len")
            fi
        fi
    done < "input.txt"

    # Add the final boundary (index *after* the last element of the last map)
    map_boundaries+=("${#all_src_starts[@]}")

    local min_location=-1
    local seed current_number location
    local map_idx map_start_index map_end_index_exclusive range_idx
    local src_start dest_start length src_end offset
    local converted

    for seed in "${seeds[@]}"; do
        current_number=$seed

        # Loop through each map defined by map_boundaries
        # The number of maps is ${#map_boundaries[@]} - 2 (excluding the initial 0 and the final end marker)
        # But using current_map_index which counts maps from 0 is simpler
        for (( map_idx=0; map_idx <= current_map_index; map_idx++ )); do
            map_start_index=${map_boundaries[map_idx+1]} # Use map_idx+1 because map_boundaries has the initial 0
            map_end_index_exclusive=${map_boundaries[map_idx+2]}

            location=$current_number # Default if no range matches in this map
            converted=false

            # Loop through ranges within the current map
            for (( range_idx=map_start_index; range_idx < map_end_index_exclusive; range_idx++ )); do
                src_start=${all_src_starts[range_idx]}
                dest_start=${all_dest_starts[range_idx]}
                length=${all_lengths[range_idx]}
                src_end=$(( src_start + length ))

                if (( current_number >= src_start && current_number < src_end )); then
                    offset=$(( current_number - src_start ))
                    location=$(( dest_start + offset ))
                    converted=true
                    break # Found match in this map, process next map
                fi
            done
            current_number=$location # Update number for the next map
        done

        if (( min_location == -1 || current_number < min_location )); then
            min_location=$current_number
        fi
    done

    echo "$min_location"
}

main
